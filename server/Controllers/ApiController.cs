using System;
using System.IO;
using System.Net;
using System.Net.Http;
using System.Net.Http.Headers;
using System.Security.Claims;
using System.Text;
using System.Text.Json;
using System.Text.Json.Serialization;
using System.Threading.Tasks;
using Meets.Services;
using Microsoft.AspNetCore.Authentication;
using Microsoft.AspNetCore.Authorization;
using Microsoft.AspNetCore.Mvc;
using Microsoft.Extensions.Logging;

namespace Meets.Controllers
{
    public static class UserExtensions
    {
        public static string Id(this ClaimsPrincipal p) =>
            p.FindFirstValue("sub");
    }

    public class Appointment
    {
        [JsonPropertyName("hostId")]
        public string HostId { get; set; }

        [JsonPropertyName("name")]
        public string Name { get; set; }

        [JsonPropertyName("start")]
        public long Start { get; set; }

        [JsonPropertyName("dur")]
        public int Dur { get; set; }

        [JsonIgnore]
        public bool Valid => Guid.TryParse(HostId, out var _) && Dur > 0;
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
            Appointment appt)
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
                    JsonSerializer.Serialize(appt),
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

            return Created($"api/bookings", appt);
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