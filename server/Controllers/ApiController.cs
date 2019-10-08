using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.ComponentModel.DataAnnotations;
using System.Globalization;
using System.Linq;
using System.Net.Http;
using System.Net.Http.Headers;
using System.Security.Claims;
using System.Text;
using System.Threading.Tasks;
using Microsoft.AspNetCore.Authentication;
using Microsoft.AspNetCore.Authorization;
using Microsoft.AspNetCore.Mvc;
using Newtonsoft.Json;

namespace PublicCallers.Controllers
{
    public static class UserExtensions
    {
        public static string Id(this ClaimsPrincipal p) =>
            p.FindFirstValue("sub");
    }

    public class BookingRequest
    {
        [Required(ErrorMessage = "HostId is required")]
        public Guid hostId { get; set; }

        [Required(ErrorMessage = "Time start is required")]
        public long start { get; set; }

        public string timeId { get; set; }
    }

    [ApiController]
    public class ApiController : ControllerBase
    {
        private readonly IHttpClientFactory _clientFactory;
        private HttpClient Client => _clientFactory.CreateClient("broker");

        public ApiController(
            IHttpClientFactory clientFactory
        )
        {
            _clientFactory = clientFactory;
        }

        [Authorize]
        [HttpGet("api/bookings")]
        public async Task<ActionResult> GetBookings()
        {
            var t = await HttpContext.GetTokenAsync("access_token");

            var req = new HttpRequestMessage
            {
                Method = HttpMethod.Get,
                RequestUri = new Uri("bookings", UriKind.Relative)
            };
            req.SetBearerToken(t);

            var res = await Client.SendAsync(req);
            Response.StatusCode = (int)res.StatusCode;

            return File(await res.Content.ReadAsStreamAsync(), "application/json");
        }

        [Authorize]
        [HttpPost("api/bookings")]
        public async Task<ActionResult> Book(
            BookingRequest b)
        {
            if (ModelState.IsValid)
            {
                var t = await HttpContext.GetTokenAsync("access_token");

                var req = new HttpRequestMessage
                {
                    Method = HttpMethod.Post,
                    RequestUri = new Uri("bookings", UriKind.Relative),
                    Content = new StringContent(
                        JsonConvert.SerializeObject(b),
                        Encoding.UTF8,
                        "application/json"
                    )
                };
                req.SetBearerToken(t);

                var res = await Client.SendAsync(req);
                res.EnsureSuccessStatusCode();

                return Created($"api/bookings", b);
            }
            else
            {
                return new BadRequestObjectResult(ModelState);
            }
        }

        [Authorize]
        [HttpDelete("api/bookings/{start}")]
        public async Task<ActionResult> UnBook(
            long start)
        {
            var t = await HttpContext.GetTokenAsync("access_token");

            var req = new HttpRequestMessage
            {
                Method = HttpMethod.Delete,
                RequestUri = new Uri($"bookings/{start}", UriKind.Relative),
            };
            req.SetBearerToken(t);

            var res = await Client.SendAsync(req);
            res.EnsureSuccessStatusCode();

            return NoContent();
        }
    }
}