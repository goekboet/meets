using System;
using System.Collections.Generic;
using System.Linq;

namespace PublicCallers.Scheduling
{
    public class Meet
    {
        public Meet(
            long start,
            string name,
            string host,
            int dur)
        {
            Start = start;
            Name = name;
            Host = host;
            Dur = dur;
        }

        public long Start { get; }
        public string Name { get; }
        public string Host { get; }
        public int Dur { get; }
    }

    public enum Weekday : byte
    {
        Mon = 1,
        Tue = 2,
        Wed = 3,
        Thu = 4,
        Fri = 5,
        Sat = 6,
        Sun = 7
    }

    public class TimeRecord
    {
        public TimeRecord(
            int year,
            int week,
            Weekday day,
            int h,
            int m,
            int dur)
        {
            Year = year;
            Week = week;
            Day = day;
            Hour = h;
            Minute = m;
        }

        public int Year { get; }
        public int Week { get; }
        public Weekday Day { get; }
        public int Hour { get; }
        public int Minute { get; }
        public int Dur { get; }

        public override string ToString() =>
            $"{Year}-{Week}-{Day} {Hour}:{Minute} ({Dur} min)";
    }

    public class TimeData
    {
        public TimeData(
            long start,
            long end,
            string record,
            int year,
            int week,
            Weekday weekday,
            int dur,
            int hour,
            int minute)
        {
            Start = start;
            End = end;
            Record = record;
            Dur = dur;
            Hour = hour;
            Minute = minute; 
        }

        public long Start { get; }
        public long End { get; }
        public string Record { get; }
        public int Dur { get; }
        public int Hour { get; }
        public int Minute { get; }
    }


    public static class SchedulingExtensions
    {
        public static Weekday FromDayOfWeek(DayOfWeek dow)
        {
            switch (dow)
            {
                case DayOfWeek.Monday: return Weekday.Mon;
                case DayOfWeek.Tuesday: return Weekday.Thu;
                case DayOfWeek.Wednesday: return Weekday.Wed;
                case DayOfWeek.Thursday: return Weekday.Thu;
                case DayOfWeek.Friday: return Weekday.Fri;
                case DayOfWeek.Saturday: return Weekday.Sat;
                case DayOfWeek.Sunday: return Weekday.Sun;

                default: throw new ArgumentOutOfRangeException(nameof(dow));
            }
        }

        public static int LastMonth(int month) => month == 1 ? 12 : month - 1;

        public static bool IsLeapYear(int year)
        {
            if (year % 4 != 0)
            {
                return false;
            }
            else if (year % 100 != 0)
            {
                return true;
            }
            else if (year % 400 != 0)
            {
                return false;
            }
            else
            {
                return true;
            }
        }

        public static int DaysInYear(int year) => IsLeapYear(year) ? 366 : 365;

        public static int DaysOfMonth(bool isLeap, int month)
        {
            switch (month)
            {
                case 1: return 31;
                case 2: return isLeap ? 29 : 28;
                case 3: return 31;
                case 4: return 30;
                case 5: return 31;
                case 6: return 30;
                case 7: return 31;
                case 8: return 31;
                case 9: return 30;
                case 10: return 31;
                case 11: return 30;
                case 12: return 31;
                default: throw new ArgumentOutOfRangeException(nameof(month));
            }
        }

        public static HashSet<int> LongYears = new HashSet<int>(
            new[]
            {
                2004,2009,2015, 2020,2026,
                2032,2037,2043, 2048,2054,
                2060,2065,2071, 2076,2082,
                2088,2093,2099, 2105,2111,
                2116,2122,2128, 2133,2139, 
                2144,2150,2156, 2161,2167, 
                2172,2178,2184, 2189,2195, 
                2201,2207,2212, 2218,2224,
                2229,2235,2240, 2246,2252,
                2257,2263,2268, 2274,2280,
                2285,2291,2296, 2303,2308,
                2314,2320,2325, 2331,2336,
                2342,2348,2353, 2359,2364,
                2370,2376,2381, 2387,2392,
                2398
            }
        );

        public static int WeeksInYear(
            int year) => LongYears.Contains(year)
                ? 53
                : 52;

        public static TimeRecord AddWeeks(
            this TimeRecord s,
            int weeks)
        {
            var lastWeek = WeeksInYear(s.Year);
            var nextyear = (s.Week + weeks) > lastWeek 
                ? s.Year + 1
                : s.Year;

            var w = s.Week + weeks;
            var nextWeek = w > lastWeek
                ? w - lastWeek
                : w;

            return new TimeRecord(nextyear, nextWeek, s.Day, s.Hour, s.Minute, 45);
        }

        public static (int year, int month, int dayOfMonth) ToGregorian(
            this TimeRecord s,
            TimeZoneInfo tz)
        {
            var Jan4Date = new DateTime(s.Year, 1, 4);
            var d = FromDayOfWeek(
                    new DateTimeOffset(
                        Jan4Date,
                        tz.GetUtcOffset(Jan4Date))
                    .DayOfWeek);

            var correction = (int)d + 3;
            var ordinal = (s.Week * 7) + (int)s.Day - correction;

            var y = ordinal < 1
                ? s.Year - 1
                : s.Year;

            if (ordinal < 1)
            {
                ordinal += DaysInYear(y);
            }
            else if (ordinal > DaysInYear(y))
            {
                ordinal -= DaysInYear(y);
            }

            int month = 0;
            int day = 0;
            var o = ordinal;
            var isLeap = IsLeapYear(y);
            while (o > 0)
            {
                month++;
                day = o;
                o -= DaysOfMonth(isLeap, month);
            }

            return (y, month, day);
        }

        private static DateTimeOffset GetDate(
            this TimeRecord s,
            TimeZoneInfo tz)
        {
            var (y, m, d) = ToGregorian(s, tz);
            var date = new DateTime(y,m,d);

            return new DateTimeOffset(
                date,
                tz.GetUtcOffset(date));
        }

        public static long ToPosix(
            this TimeRecord s,
            TimeZoneInfo tz) => s.GetDate(tz)
            .ToUnixTimeMilliseconds();

        public static TimeData ToTimeData(
            this TimeRecord t,
            TimeZoneInfo tz)
        {
            var start = t.ToPosix(tz);
            var end = start + (t.Dur * 60 * 1000);
            var record = t.ToString();

            return new TimeData(
                start,
                end,
                record,
                t.Year,
                t.Week,
                t.Day,
                t.Dur,
                t.Hour,
                t.Minute
            );
        }

        public static Meet ToMeet(
            this TimeData d,
            string host) => new Meet(
                d.Start,
                d.Record,
                host,
                d.Dur
            );

        public static IEnumerable<TimeRecord> RepeatWeekly(
            TimeRecord r,
            int n
        ) => Enumerable.Range(0, n)
            .Select(x => r.AddWeeks(x));

        public static IEnumerable<TimeRecord> DailySchedule(
            int year,
            int week,
            Weekday day,
            IEnumerable<(int h, int m, int dur)> times
        ) => times
            .Select(x => new TimeRecord(year, week, day, x.h, x.m, x.dur));

        public static IEnumerable<TimeRecord> WeeklySchedule(
            int year,
            int week,
            Dictionary<Weekday, IEnumerable<(int h, int m, int dur)>> daily,
            int r
        ) => daily
            .SelectMany(x => DailySchedule(year, week, x.Key, x.Value))
            .SelectMany(x => RepeatWeekly(x, r));

        public static IEnumerable<Meet> GetMeets(
            this TimeRecord s,
            long from,
            long to)
        {
            return new Meet[0];
        }
    }
}