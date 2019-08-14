using System;
using System.Diagnostics;
using System.Net.Http;
using System.Threading.Tasks;
using Microsoft.AspNetCore.Authentication;
using Microsoft.AspNetCore.Authorization;
using Microsoft.AspNetCore.Mvc;
using Microsoft.Extensions.Logging;
using PublicCallers.Models;

namespace PublicCallers.Controllers
{
    public class AppState
    {
        public bool HasCreds { get; set; }
    }

    public class HomeController : Controller
    {
        public ILogger<HomeController> Logger { get; }

        public HomeController(
            ILogger<HomeController> logger)
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


        [ResponseCache(Duration = 0, Location = ResponseCacheLocation.None, NoStore = true)]
        public IActionResult Error()
        {
            return View(new ErrorViewModel { RequestId = Activity.Current?.Id ?? HttpContext.TraceIdentifier });
        }
    }
}
