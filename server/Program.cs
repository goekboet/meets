using Microsoft.AspNetCore;
using Microsoft.AspNetCore.Hosting;
using Microsoft.IdentityModel.Logging;
using Serilog;

namespace PublicCallers
{
    public class Program
    {
        public static void Main(string[] args)
        {
            CreateWebHostBuilder(args).Build().Run();
        }

        public static IWebHostBuilder CreateWebHostBuilder(string[] args) =>
            WebHost.CreateDefaultBuilder(args)
                .UseStartup<Startup>()
                .UseSerilog((context, configuration) =>
                    {
                        configuration
                            .ReadFrom.Configuration(context.Configuration);
                        IdentityModelEventSource.ShowPII = true;
                    });
    }
}
