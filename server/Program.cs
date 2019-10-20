using System;
using Microsoft.IdentityModel.Logging;
using Microsoft.AspNetCore.Hosting;
using Microsoft.Extensions.Hosting;
using Serilog;
using Serilog.Formatting.Elasticsearch;

namespace Meets
{
    public class Program
    {
        public static void Main(string[] args)
        {
            CreateHostBuilder(args).Build().Run();
        }

        public static LoggerConfiguration SwitchLogger(
            string key, 
            LoggerConfiguration logger)
        {
            switch (key)
            {
                case "Console":
                    logger.WriteTo.Console();
                    break;
                case "StdOutJson":
                    logger.WriteTo.Console(new ElasticsearchJsonFormatter());
                    break;
                default:
                    throw new ArgumentOutOfRangeException($"key: {key}");
            }

            return logger;
        }

        public static IHostBuilder CreateHostBuilder(string[] args) =>
            Host.CreateDefaultBuilder(args)
                .ConfigureWebHostDefaults(webBuilder =>
        {
            webBuilder
                .UseStartup<Startup>()
                .UseSerilog((context, configuration) =>
                    {
                        IdentityModelEventSource.ShowPII = true;
                        var key = context.Configuration["Serilog:Configuration"];
                        SwitchLogger(key, configuration);
                    });
        });
                
    }
}
