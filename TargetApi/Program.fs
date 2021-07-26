namespace TargetApi
open System.IO
open Microsoft.AspNetCore.Hosting
open Microsoft.Extensions.Hosting

module Program =
    let exitCode = 0

    let CreateHostBuilder args =
        Host.CreateDefaultBuilder(args)
            .ConfigureWebHostDefaults(fun webBuilder ->
                webBuilder.UseStartup<Startup>() |> ignore
            )
            .UseContentRoot(Directory.GetCurrentDirectory())

    [<EntryPoint>]
    let main args =
        CreateHostBuilder(args).Build().Run()

        exitCode
