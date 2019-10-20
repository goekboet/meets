using Microsoft.AspNetCore.Authentication;
using Microsoft.AspNetCore.Mvc;
using Microsoft.Extensions.Logging;

namespace Meets.Controllers
{
    public class AppState
    {
        public bool HasCreds { get; set; }
    }

    public class SpaController : Controller
    {
        public ILogger<SpaController> Logger { get; }

        public SpaController(
            ILogger<SpaController> logger)
        {
            Logger = logger;
        }

        public IActionResult Index()
        {
            return View(new AppState { HasCreds = User.Identity.IsAuthenticated });
        }

        [HttpGet("/login")]
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
