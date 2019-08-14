using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.ComponentModel.DataAnnotations;
using System.Globalization;
using System.Linq;
using System.Security.Claims;
using Microsoft.AspNetCore.Authorization;
using Microsoft.AspNetCore.Mvc;

namespace PublicCallers.Controllers
{
    public static class UserExtensions
    {
        public static string Id(this ClaimsPrincipal p) => 
            p.FindFirstValue("sub");
    }

    public class Booking
    {
        [Required(ErrorMessage = "Schedule is required")]
        public string Schedule { get; set; }
        
        [Required(ErrorMessage = "Meet name is required")]
        public string Name { get; set; }
        public long Time { get; set; }
        public int Duration { get; set; }
    }

    [ApiController]
    public class ApiController : ControllerBase
    {
        static readonly TimeZoneInfo hostTz =
            TimeZoneInfo.FindSystemTimeZoneById("America/Los_Angeles");

        public static DateTimeOffset HostWeekStart(
            DateTimeOffset d,
            TimeZoneInfo tz)
        {
            var hostLocal = TimeZoneInfo.ConvertTime(d, tz);
            var monday = ToStartOfDay(GetMonday(hostLocal));

            return monday;
        }

        public static DateTimeOffset ToStartOfDay(DateTimeOffset d) =>
            d.Subtract(d.TimeOfDay);
        public static DateTimeOffset GetMonday(DateTimeOffset d)
        {
            switch (d.DayOfWeek)
            {
                case DayOfWeek.Tuesday:
                    return d.AddDays(-1);
                case DayOfWeek.Wednesday:
                    return d.AddDays(-2);
                case DayOfWeek.Thursday:
                    return d.AddDays(-3);
                case DayOfWeek.Friday:
                    return d.AddDays(-4);
                case DayOfWeek.Saturday:
                    return d.AddDays(-5);
                case DayOfWeek.Sunday:
                    return d.AddDays(-6);
                default:
                    return d;
            }
        }
        public static int WeekNo(DateTimeOffset time)
        {
            DayOfWeek day = CultureInfo.InvariantCulture.Calendar.GetDayOfWeek(time.DateTime);
            if (day >= DayOfWeek.Monday && day <= DayOfWeek.Wednesday)
            {
                time = time.AddDays(3);
            }

            // Return the week of our adjusted day
            return CultureInfo.InvariantCulture.Calendar.GetWeekOfYear(time.DateTime, CalendarWeekRule.FirstFourDayWeek, DayOfWeek.Monday);
        }

        public static string DayName(DateTimeOffset t) => t.DayOfWeek.ToString().Substring(0, 3);

        static string ZeroPaddedH(DateTimeOffset t) => t.Hour.ToString().PadLeft(2, '0');


        static IEnumerable<DateTimeOffset> EverySevenDays(DateTimeOffset d)
        {
            var next = d.AddDays(7);
            yield return d;
            while (true)
            {
                yield return next;
                next = next.AddDays(7);
            }
        }

        public static object[] MockMeetings(
            string schedule,
            DateTimeOffset from,
            DateTimeOffset to,
            HashSet<long> booked)
        {
            var lowBound = HostWeekStart(from, hostTz)
                    .Subtract(TimeSpan.FromDays(7));

            return EverySevenDays(lowBound)
                    .TakeWhile(x => x <= to)
                    .SelectMany(x =>
                        Enumerable
                            .Range(10, 8)
                            .Select(h => x.AddHours(h)))
                    .SelectMany(x =>
                        Enumerable
                            .Range(0, 5)
                            .Select(d => x.AddDays(d)))
                    .OrderBy(x => x)
                    .SkipWhile(x => x < from)
                    .Select(x => new
                    {
                        schedule = schedule,
                        name = $"{DayName(x)}-W{WeekNo(x)}-{ZeroPaddedH(x)}",
                        time = x.ToUnixTimeMilliseconds(),
                        duration = 45
                    })
                    .Where(x => !booked.Contains(x.time))
                    .ToArray();
        }

        [HttpGet("api/schedules")]
        public ActionResult<object[]> List()
        {
            return Ok(new[]
            {
                new { name = "First!"},
                new { name = "Second-to-none"},
                new { name = "Thirds the charm"},
                new { name = "may the Fourth be with you"},
                new { name = "Fifth"},
                new { name = "Sixth"},
                new { name = "Seventh"},
                new { name = "Eight makes Reight"},
                new { name = "Nineth"},
                new { name = "Tenth"}
            });
        }

        static ConcurrentDictionary<string, List<Booking>> Bookings =
            new ConcurrentDictionary<string, List<Booking>>();

        static ConcurrentDictionary<string, HashSet<long>> Booked = 
            new ConcurrentDictionary<string, HashSet<long>>();

        [HttpGet("api/schedules/{name}")]
        public ActionResult<object> Get(string name)
        {
            var now = ToStartOfDay(GetMonday(DateTimeOffset.UtcNow));
            var booked = Booked.GetValueOrDefault(
                name, 
                new HashSet<long>());

            return Ok(new
            {
                name = name,
                host = "someHost",
                meets = MockMeetings(name, now, now.AddDays(7), booked)
            });
        }

        [Authorize]
        [HttpGet("api/bookings")]
        public ActionResult GetBookings()
        {   
            var bs = Bookings.GetValueOrDefault(User.Id(), new List<Booking>(0));
            
            return Ok(bs.Select(x => new
            {
                time = x.Time,
                name = x.Name,
                schedule = x.Schedule,
                duration = x.Duration
            }));
        }

        [Authorize]
        [HttpGet("api/bookings/{id}")]
        public ActionResult GetBooking(long t)
        {
            Booking b = null;
            if (Bookings.TryGetValue(User.Id(), out var bs))
            {
                b = bs.FirstOrDefault(x => x.Time == t);
            }

            return Ok(b != null 
                ? new 
                    { 
                        schedule = b.Schedule,
                        time = b.Time,
                        name = b.Name,
                        dur = b.Duration
                    } 
                : new object());
        }

        [Authorize]
        [HttpPost("api/bookings")]
        public ActionResult Book(Booking b)
        {
            if (ModelState.IsValid)
            {
                var success = Bookings.AddOrUpdate(
                User.Id(),
                new List<Booking>() { b },
                (sub, booked) =>
                {
                    booked.Add(b);
                    return booked;
                });

                Booked.AddOrUpdate(
                    b.Schedule,
                    new HashSet<long>(),
                    (s, bs) => {
                        bs.Add(b.Time);
                        return bs;
                    }
                );

                return Created($"api/bookings", b);
            }
            else
            {
                return new BadRequestObjectResult(ModelState);
            }
            
        }
    }
}