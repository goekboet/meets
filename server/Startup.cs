using System.IdentityModel.Tokens.Jwt;
using System.Net.Http.Headers;
using Microsoft.AspNetCore.Builder;
using Microsoft.AspNetCore.Hosting;
using Microsoft.AspNetCore.Http;
using Microsoft.AspNetCore.HttpOverrides;
using Microsoft.AspNetCore.Mvc;
using Microsoft.Extensions.Configuration;
using Microsoft.Extensions.DependencyInjection;

namespace PublicCallers
{
    public class Startup
    {
        public static readonly string CookieScheme = "Cookies";
        public static readonly string OpenIdScheme = "oidc";

        public Startup(IConfiguration configuration)
        {
            Configuration = configuration;
        }

        public IConfiguration Configuration { get; }

        // This method gets called by the runtime. Use this method to add services to the container.
        public void ConfigureServices(IServiceCollection services)
        {
            // Sets up meets to run behind reverse proxy.
            // https://docs.microsoft.com/en-us/aspnet/core/host-and-deploy/proxy-load-balancer?view=aspnetcore-2.2
            services.Configure<ForwardedHeadersOptions>(options =>
            {
                options.ForwardedHeaders = ForwardedHeaders.XForwardedFor |
                    ForwardedHeaders.XForwardedProto;
                
                // Per default kestrel only forwards proxy-headers from localhost. You need to add
                // ip-numbers into these lists or empty them to forward all.
                options.KnownNetworks.Clear();
                options.KnownProxies.Clear();
            });

            services.Configure<CookiePolicyOptions>(options =>
            {
                // This lambda determines whether user consent for non-essential cookies is needed for a given request.
                options.CheckConsentNeeded = context => true;
                options.MinimumSameSitePolicy = SameSiteMode.None;
            });


            services.AddMvc().SetCompatibilityVersion(CompatibilityVersion.Version_2_2);

            JwtSecurityTokenHandler.DefaultInboundClaimTypeMap.Clear();

            services.AddAuthentication(options =>
                {
                    options.DefaultScheme = CookieScheme;
                    options.DefaultChallengeScheme = OpenIdScheme;
                })
                .AddCookie("Cookies", options => 
                {
                    options.Cookie.Name = "ego.meets";
                })
                .AddOpenIdConnect(OpenIdScheme, options =>
                {
                    Configuration.GetSection("Ids").Bind(options);
                    options.ResponseType = "code";
                    options.GetClaimsFromUserInfoEndpoint = true;
                    options.SaveTokens = true;
                    options.Scope.Add("bookings");
                    options.Scope.Add("profile");
                });

            services.AddHttpClient("broker", opts =>
            {
                Configuration.GetSection("Broker").Bind(opts);
                opts.DefaultRequestHeaders.Accept.Add(
                    new MediaTypeWithQualityHeaderValue("application/json"));
            });
        }

        // This method gets called by the runtime. Use this method to configure the HTTP request pipeline.
        public void Configure(IApplicationBuilder app, IHostingEnvironment env)
        {
            app.UseForwardedHeaders();
            
            app.UseDeveloperExceptionPage();
            app.UseAuthentication();
            app.UseStaticFiles();
            app.UseCookiePolicy();

            app.UseMvc(routes =>
                routes.MapSpaFallbackRoute(
                    name: "elm-app",
                    defaults: new { controller = "Home", action = "Index" }
                ));
            
        }
    }
}
