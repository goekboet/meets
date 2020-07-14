using Microsoft.AspNetCore.Authentication;
using Microsoft.AspNetCore.Mvc;
using Microsoft.Extensions.Logging;

namespace Meets.Controllers
{
    public class AppState
    {
        public string Name { get; set; }
    }

    public class SpaController : Controller
    {
        public ILogger<SpaController> Logger { get; }

        string Name => User.Identity.IsAuthenticated
            ? User.FindFirst(x => x.Type == "name")?.Value ?? "n/a"
            : null;

        public SpaController(
            ILogger<SpaController> logger)
        {
            Logger = logger;
        }

        public IActionResult Index() => View(
            new AppState 
            { 
                Name = Name 
            });

        [HttpPost("/login")]
        [ValidateAntiForgeryToken]
        public IActionResult Login(string sparoute)
        {
            if (sparoute == null)
            {
                Logger.LogWarning("Received null spa-route. Will redirect to ~/.");
                sparoute = "";
            }
            if (User.Identity.IsAuthenticated)
            {
                return Redirect(sparoute);
            }
            else
            {
                return Challenge(new AuthenticationProperties
                {
                    RedirectUri = sparoute
                });
            }
        }

        [HttpPost("/logout")]
        [ValidateAntiForgeryToken]
        public IActionResult Logout(string sparoute)
        {
            if (sparoute == null)
            {
                Logger.LogWarning("Received null spa-route. Will redirect to ~/.");
                sparoute = "";
            }
            if (!User.Identity.IsAuthenticated)
            {
                return Redirect(sparoute);
            }

            return new SignOutResult(new[]
                {
                    Startup.CookieScheme,
                    Startup.OpenIdScheme
                }, new AuthenticationProperties
                {
                    RedirectUri = sparoute
                });
        }
    }
}
