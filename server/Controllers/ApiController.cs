using System;
using System.ComponentModel.DataAnnotations;
using System.IO;
using System.Net;
using System.Net.Http;
using System.Net.Http.Headers;
using System.Security.Claims;
using System.Text;
using System.Threading.Tasks;
using Meets.Services;
using Microsoft.AspNetCore.Authentication;
using Microsoft.AspNetCore.Authorization;
using Microsoft.AspNetCore.Mvc;
using Microsoft.Extensions.Logging;
using Newtonsoft.Json;

namespace Meets.Controllers
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
        private readonly IBookings _client;
        private readonly ILogger<ApiController> _log;

        public ApiController(
            IBookings client,
            ILogger<ApiController> log
        )
        {
            _client = client;
            _log = log;
        }

        [Authorize]
        [HttpGet("api/bookings")]
        public async Task<ActionResult<Stream>> GetBookings()
        {
            var t = await HttpContext.GetTokenAsync("access_token");
            if (t == null)
            {
                _log.LogInformation("Found no accesstoken for {User}", User.Id() ?? "n/a");
                return Unauthorized();
            }

            var req = new HttpRequestMessage
            {
                Method = HttpMethod.Get,
                RequestUri = new Uri("bookings", UriKind.Relative)
            };
            req.Headers.Authorization = new AuthenticationHeaderValue("Bearer", t);

            var res = await _client.List(req);
            if (res.StatusCode == HttpStatusCode.Unauthorized)
            {
                _log.LogInformation("Accesstoken was rejected for {User}.", User.Id() ?? "n/a");
                return Unauthorized();
            }
            else
            {
                res.EnsureSuccessStatusCode();
            }
            Response.StatusCode = (int)res.StatusCode;

            return await res.Content.ReadAsStreamAsync();
        }

        [Authorize]
        [HttpPost("api/bookings")]
        public async Task<ActionResult> Book(
            BookingRequest b)
        {
            if (ModelState.IsValid)
            {
                var t = await HttpContext.GetTokenAsync("access_token");
                if (t == null)
                {
                    _log.LogInformation("Found no accesstoken for {User}", User.Id() ?? "n/a");
                    return Unauthorized();
                }

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
                req.Headers.Authorization = new AuthenticationHeaderValue("Bearer", t);

                var res = await _client.Post(req);
                if (res.StatusCode == HttpStatusCode.Unauthorized)
                {
                    _log.LogInformation("Accesstoken was rejected for {User}.", User.Id() ?? "n/a");
                    return Unauthorized();
                }
                else
                {
                    res.EnsureSuccessStatusCode();
                }

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
            if (t == null)
            {
                _log.LogInformation("Found no accesstoken for {User}", User.Id() ?? "n/a");
                return Unauthorized();
            }

            var req = new HttpRequestMessage
            {
                Method = HttpMethod.Delete,
                RequestUri = new Uri($"bookings/{start}", UriKind.Relative),
            };
            req.Headers.Authorization = new AuthenticationHeaderValue("Bearer", t);

            var res = await _client.Delete(req);
            if (res.StatusCode == HttpStatusCode.Unauthorized)
            {
                _log.LogInformation("Accesstoken was rejected for {User}.", User.Id() ?? "n/a");
                return Unauthorized();
            }
            else
            {
                res.EnsureSuccessStatusCode();
            }

            return NoContent();
        }
    }
}