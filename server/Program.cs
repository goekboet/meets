using System;
using Microsoft.AspNetCore;
using Microsoft.AspNetCore.Hosting;
using Microsoft.IdentityModel.Logging;
using Serilog;
using Serilog.Formatting.Elasticsearch;

namespace PublicCallers
{
    public class Program
    {
        public static void Main(string[] args)
        {
            CreateWebHostBuilder(args).Build().Run();
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

        public static IWebHostBuilder CreateWebHostBuilder(string[] args) =>
            WebHost.CreateDefaultBuilder(args)
                .UseStartup<Startup>()
                .UseSerilog((context, configuration) =>
                    {
                        IdentityModelEventSource.ShowPII = true;
                        var key = context.Configuration["Serilog:Configuration"];
                        SwitchLogger(key, configuration);
                    });
    }
}
