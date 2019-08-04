using System;
using System.Collections.Generic;
using System.Globalization;
using System.Linq;
using Microsoft.AspNetCore.Authorization;
using Microsoft.AspNetCore.Mvc;

namespace PublicCallers.Controllers
{
    public class ApiController : ControllerBase
    {
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

        public static string DayName(DateTimeOffset t) => t.DayOfWeek.ToString();

        static object[] cache = null;
        public static object[] MockMeetings()
        {
            if (null == cache)
            {
                var refDate = new DateTimeOffset(new DateTime(2019, 7, 29, 10, 0, 0));
                var days = Enumerable.Range(0, 7)
                    .Select(x => refDate.AddHours(x));

                var week = Enumerable.Range(0, 7)
                    .SelectMany(x => days.Select(d => d.AddDays(x)));

                cache = week
                    .Select(x => new
                    {
                        name = $"{DayName(x)}-{WeekNo(x)}",
                        time = x.ToUnixTimeMilliseconds(),
                        dur = 45
                    }).ToArray();
            }

            return cache;
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

        [HttpGet("api/schedules/{name}")]
        public ActionResult<object> Get(string name)
        {
            return Ok(new
            {
                name = name,
                host = "someHost",
                meets = MockMeetings()
            });
        }


        [Authorize]
        [HttpPost("api/schedules/{schedule}/time/{time}")]
        public ActionResult Book(string schedule, string time)
        {
            return Created($"api/schedules/{schedule}/time/{time}", new object());
        }
    }
}