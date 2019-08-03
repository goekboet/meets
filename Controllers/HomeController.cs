using System.Diagnostics;
using System.Net.Http;
using System.Threading.Tasks;
using Microsoft.AspNetCore.Authorization;
using Microsoft.AspNetCore.Mvc;
using PublicCallers.Models;

namespace PublicCallers.Controllers
{
    public class AppState
    {
        public bool HasCreds { get; set; }
    }

    public class HomeController : Controller
    {
        public static HttpClient Client = new HttpClient();



        public IActionResult Index()
        {
            return View(new AppState { HasCreds = User.Identity.IsAuthenticated });
        }

        public IActionResult Login()
        {
            if (User.Identity.IsAuthenticated)
            {
                return Redirect("~/");
            }
            else
            {
                return Challenge();
            }
        }

        [ResponseCache(Duration = 0, Location = ResponseCacheLocation.None, NoStore = true)]
        public IActionResult Error()
        {
            return View(new ErrorViewModel { RequestId = Activity.Current?.Id ?? HttpContext.TraceIdentifier });
        }
    }
}
