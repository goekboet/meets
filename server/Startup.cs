using System.IdentityModel.Tokens.Jwt;
using System.IO;
using System.Security.Cryptography.X509Certificates;
using IdentityModel;
using Meets.RefreshTokenHandling;
using Meets.Services;
using Microsoft.AspNetCore.Authentication;
using Microsoft.AspNetCore.Builder;
using Microsoft.AspNetCore.DataProtection;
using Microsoft.AspNetCore.Http;
using Microsoft.AspNetCore.HttpOverrides;
using Microsoft.Extensions.Configuration;
using Microsoft.Extensions.DependencyInjection;
using Microsoft.IdentityModel.Tokens;

namespace Meets
{
    public class Startup
    {
        public static readonly string CookieScheme = "Cookies";
        public static readonly string OpenIdScheme = "oidc";

        public Startup(IConfiguration configuration)
        {
            Configuration = configuration;
            JwtSecurityTokenHandler.DefaultInboundClaimTypeMap.Clear();
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


            services.AddControllersWithViews();

            services.AddAuthentication(options =>
                {
                    options.DefaultScheme = CookieScheme;
                    options.DefaultChallengeScheme = OpenIdScheme;
                })
                .AddCookie("Cookies", options => 
                {
                    options.Cookie.Name = "ego.meets";
                    options.AccessDeniedPath = new PathString("/");
                })
                .AddAutomaticTokenManagement()
                .AddOpenIdConnect(OpenIdScheme, options =>
                {
                    Configuration.GetSection("Ids").Bind(options);
                    options.ResponseType = "code";
                    options.GetClaimsFromUserInfoEndpoint = true;
                    options.SaveTokens = true;
                    options.Scope.Add("bookings");
                    options.Scope.Add("profile");
                    options.Scope.Add("offline_access");
                    
                    options.ClaimActions.MapAllExcept("iss", "nbf", "exp", "aud", "nonce", "iat", "c_hash");

                    options.TokenValidationParameters = new TokenValidationParameters
                    {
                        NameClaimType = JwtClaimTypes.Name,
                        RoleClaimType = JwtClaimTypes.Role,
                    };

                    options.AccessDeniedPath = new PathString("/");
                });
            
            services.AddBrokerClient(Configuration);

            if (Configuration["Dataprotection:Type"] == "Docker")
            {
                services.AddDataProtection()
                    .PersistKeysToFileSystem(
                        new DirectoryInfo(Configuration["Dataprotection:KeyPath"])
                    )
                    .ProtectKeysWithCertificate(
                        new X509Certificate2(
                            Configuration["Dataprotection:CertPath"],
                            Configuration["Dataprotection:CertPass"]
                        )
                    );
            }
        }

        // This method gets called by the runtime. Use this method to configure the HTTP request pipeline.
        public void Configure(IApplicationBuilder app)
        {
            app.UseForwardedHeaders();
            app.UseStaticFiles();
            app.UseRouting();
            
            app.UseDeveloperExceptionPage();
            app.UseAuthentication();
            app.UseAuthorization();
            app.UseCookiePolicy();

            app.UseEndpoints(endpoints => {
                endpoints.MapFallbackToController(
                    action: "Index",
                    controller: "Spa"
                );
            });
        }
    }
}
