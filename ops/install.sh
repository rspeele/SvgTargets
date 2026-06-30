#!/usr/bin/env bash
# Provisions an Ubuntu 24.04 host for SvgTargets.com.
#
# Usage (on the target VM, as a user with passwordless sudo):
#     EMAIL=you@example.com bash install.sh
#
# Optional overrides via env var:
#     DOMAIN          - hostname Let's Encrypt will issue against   (default: svgtargets.com)
#     EMAIL           - contact address for Let's Encrypt           (REQUIRED)
#     SRC_DIR         - where the git repo gets cloned               (default: $HOME/svgtargets-src)
#     APP_DIR         - where the published binaries live            (default: /opt/svgtargets)
#     SERVICE_USER    - unprivileged account that runs Kestrel       (default: svgtargets)
#     BRANCH          - git branch to deploy from                    (default: master)
#     KESTREL_PORT    - localhost port Kestrel binds                 (default: 5051)
#
# The app is stateless (it renders SVGs on the fly from query params), so there
# is no database, data directory, or connection string to manage. Re-running
# this script is safe: it fast-forwards the repo, republishes, and restarts.
#
# IMPORTANT: when co-hosting several of these sites on one box, each MUST use a
# distinct KESTREL_PORT. nginx routes by domain, but every site's Kestrel binds
# its own localhost port; if two sites share a port, only one can bind it and
# both domains end up proxying to whichever app won the port. The default here
# (5051) differs from the rzsql.com script's 5050 for exactly this reason, and
# the pre-flight check below refuses to proceed on a port another service holds.

set -euo pipefail

DOMAIN="${DOMAIN:-svgtargets.com}"
EMAIL="${EMAIL:-}"
SRC_DIR="${SRC_DIR:-$HOME/svgtargets-src}"
APP_DIR="${APP_DIR:-/opt/svgtargets}"
SERVICE_USER="${SERVICE_USER:-svgtargets}"
BRANCH="${BRANCH:-master}"
KESTREL_PORT="${KESTREL_PORT:-5051}"

REPO_URL="https://github.com/rspeele/SvgTargets.git"
PROJECT="TargetApi/TargetApi.fsproj"   # relative to the repo root
APP_DLL="TargetApi.dll"                 # produced by `dotnet publish`

if [ -z "$EMAIL" ]; then
    echo "ERROR: set EMAIL=you@example.com (used for Let's Encrypt registration)" >&2
    exit 1
fi

log() { printf '\n\033[1;36m==> %s\033[0m\n' "$*"; }

#-----------------------------------------------------------------------------#
# 0. Pre-flight: make sure KESTREL_PORT isn't already taken by another service #
#-----------------------------------------------------------------------------#
# Co-hosted sites each need a unique localhost port (see header note). Bail early
# if something other than THIS site's own service is already listening on it,
# so we never end up with two domains proxying to one backend. Our own service
# holding the port (a re-run) is fine and allowed.
# (|| true so an empty match under `set -o pipefail` doesn't abort the script)
port_holder_pid=$(sudo ss -ltnpH "sport = :$KESTREL_PORT" 2>/dev/null \
    | grep -oE 'pid=[0-9]+' | head -1 | cut -d= -f2 || true)
our_pid=$(systemctl show -p MainPID --value "$SERVICE_USER.service" 2>/dev/null || echo 0)
if [ -n "$port_holder_pid" ] && [ "$port_holder_pid" != "$our_pid" ]; then
    echo "ERROR: port $KESTREL_PORT is already in use by PID $port_holder_pid, which is not" >&2
    echo "       this site's service ($SERVICE_USER). Co-hosted sites must each use a unique" >&2
    echo "       port. Re-run with a free one, e.g.  KESTREL_PORT=5052 EMAIL=$EMAIL bash $0" >&2
    exit 1
fi

#-----------------------------------------------------------------------------#
# 1. System packages                                                          #
#-----------------------------------------------------------------------------#
log "Installing apt packages"
sudo apt-get update
sudo apt-get install -y \
    git curl ca-certificates \
    nginx \
    certbot python3-certbot-nginx \
    ufw

#-----------------------------------------------------------------------------#
# 2. .NET 10 SDK from packages.microsoft.com                                  #
#-----------------------------------------------------------------------------#
if ! dotnet --list-sdks 2>/dev/null | grep -q '^10\.'; then
    log "Installing .NET 10 SDK"
    tmpdeb=$(mktemp --suffix=.deb)
    curl -fsSL -o "$tmpdeb" \
        https://packages.microsoft.com/config/ubuntu/24.04/packages-microsoft-prod.deb
    sudo dpkg -i "$tmpdeb"
    rm -f "$tmpdeb"
    sudo apt-get update
    sudo apt-get install -y dotnet-sdk-10.0
else
    log ".NET 10 SDK already installed: $(dotnet --version)"
fi

#-----------------------------------------------------------------------------#
# 3. Clone / refresh the SvgTargets repo                                      #
#-----------------------------------------------------------------------------#
log "Cloning $REPO_URL into $SRC_DIR (branch $BRANCH)"
if [ -d "$SRC_DIR/.git" ]; then
    git -C "$SRC_DIR" fetch origin "$BRANCH"
    git -C "$SRC_DIR" checkout "$BRANCH"
    git -C "$SRC_DIR" reset --hard "origin/$BRANCH"
else
    git clone --branch "$BRANCH" "$REPO_URL" "$SRC_DIR"
fi

#-----------------------------------------------------------------------------#
# 4. Publish the web app                                                      #
#-----------------------------------------------------------------------------#
log "Publishing $PROJECT"
sudo mkdir -p "$APP_DIR"
sudo chown "$USER:$USER" "$APP_DIR"
# Clear the publish dir so stale files from a previous deploy don't linger.
rm -rf "${APP_DIR:?}/"*
dotnet publish "$SRC_DIR/$PROJECT" -c Release -o "$APP_DIR" --self-contained false

#-----------------------------------------------------------------------------#
# 5. Service account                                                          #
#-----------------------------------------------------------------------------#
log "Setting up service account"
if ! id "$SERVICE_USER" >/dev/null 2>&1; then
    sudo useradd --system --shell /usr/sbin/nologin --home "$APP_DIR" "$SERVICE_USER"
fi
sudo chown -R "$SERVICE_USER:$SERVICE_USER" "$APP_DIR"

#-----------------------------------------------------------------------------#
# 6. systemd unit                                                             #
#-----------------------------------------------------------------------------#
log "Installing systemd unit"
sudo tee "/etc/systemd/system/$SERVICE_USER.service" >/dev/null <<EOF
[Unit]
Description=SvgTargets web app
After=network.target

[Service]
WorkingDirectory=$APP_DIR
ExecStart=/usr/bin/dotnet $APP_DIR/$APP_DLL --urls http://127.0.0.1:$KESTREL_PORT
Restart=always
RestartSec=10
SyslogIdentifier=$SERVICE_USER
User=$SERVICE_USER
Environment=ASPNETCORE_ENVIRONMENT=Production
Environment=DOTNET_NOLOGO=1
ProtectSystem=full
NoNewPrivileges=true
PrivateTmp=true

[Install]
WantedBy=multi-user.target
EOF

sudo systemctl daemon-reload
sudo systemctl enable "$SERVICE_USER"
sudo systemctl restart "$SERVICE_USER"

#-----------------------------------------------------------------------------#
# 7. Firewall                                                                 #
#-----------------------------------------------------------------------------#
log "Opening firewall for HTTP/HTTPS/SSH"
sudo ufw allow OpenSSH
sudo ufw allow 'Nginx Full'
sudo ufw --force enable

#-----------------------------------------------------------------------------#
# 8. Nginx reverse proxy                                                      #
#-----------------------------------------------------------------------------#
log "Configuring nginx for $DOMAIN"
sudo tee "/etc/nginx/sites-available/$DOMAIN" >/dev/null <<EOF
server {
    listen 80;
    listen [::]:80;
    server_name $DOMAIN;

    location / {
        proxy_pass         http://127.0.0.1:$KESTREL_PORT;
        proxy_http_version 1.1;
        proxy_set_header   Upgrade \$http_upgrade;
        proxy_set_header   Connection keep-alive;
        proxy_set_header   Host \$host;
        proxy_cache_bypass \$http_upgrade;
        proxy_set_header   X-Forwarded-For \$proxy_add_x_forwarded_for;
        proxy_set_header   X-Forwarded-Proto \$scheme;
    }
}
EOF

sudo ln -sf "/etc/nginx/sites-available/$DOMAIN" "/etc/nginx/sites-enabled/$DOMAIN"
sudo rm -f /etc/nginx/sites-enabled/default
sudo nginx -t
sudo systemctl reload nginx

#-----------------------------------------------------------------------------#
# 9. Let's Encrypt                                                            #
#-----------------------------------------------------------------------------#
log "Provisioning HTTPS certificate for $DOMAIN"
sudo certbot --nginx \
    -d "$DOMAIN" \
    --non-interactive \
    --agree-tos \
    -m "$EMAIL" \
    --redirect

# certbot installs a systemd timer (certbot.timer) for auto-renewal — verify.
sudo systemctl status certbot.timer --no-pager || true

log "SvgTargets is live at https://$DOMAIN"
