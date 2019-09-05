using System;
using System.Collections.Generic;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using PublicCallers.Scheduling;
using static PublicCallers.Scheduling.Weekday;
using FirstLastOccurenceLookup = System.Collections.Generic.Dictionary<string,Tests.Schedulings.FirstLastOccurenceCase>;

namespace Tests.Schedulings
{

    public class FirstLastOccurenceCase
    {
        public TimeZoneInfo Tz { get; set; }
        public TimeRecord Given { get; set; }
        public int Recur {get; set;}
        public (long first, long last) Expect { get; set; }
    } 

    public class GetGregorianYearCase
    {
        public TimeZoneInfo Tz { get; set; }
        public TimeRecord Given { get; set; }
        public (int year, int month, int dayOfMonth) Expect { get; set; }
    }

    [TestClass]
    public class SchedulingTests
    {
        static TimeZoneInfo Stockholm = TimeZoneInfo.FindSystemTimeZoneById("Europe/Stockholm");
        public static FirstLastOccurenceLookup FirstlastOccurenceCases = new 
            FirstLastOccurenceLookup
        {
            ["MondayAtTenW35Y2019Oneoff"] = new FirstLastOccurenceCase
            {
                Tz = Stockholm,
                Given = new TimeRecord(2019, 36, Mon, 10, 0, 45),
                Recur = 1,
                Expect = (1567411200000, 1567411200000)
            },
            ["MondayAtTenW35Y2019ForTen"] = new FirstLastOccurenceCase
            {
                Tz = Stockholm,
                Given = new TimeRecord(2019, 36, Mon, 10, 0, 45),
                Recur = 3,
                Expect = (1567411200000, 1568620800000)
            },
            ["RecurOverYearEndShortYear"] = new FirstLastOccurenceCase
            {
                Tz = Stockholm,
                Given = new TimeRecord(2019, 51, Mon, 10, 0, 45),
                Recur = 3,
                Expect = (1576486800000, 1577696400000)
            },
            ["RecurOverYearEndLongYear"] = new FirstLastOccurenceCase
            {
                Tz = Stockholm,
                Given = new TimeRecord(2020, 52, Mon, 10, 0, 45),
                Recur = 3,
                Expect = (1608541200000, 1609750800000)
            }
        };

        public static Dictionary<string, GetGregorianYearCase> GetGregorianYearCases = 
            new Dictionary<string, GetGregorianYearCase>
        {
            ["2019-W01-01"] = new GetGregorianYearCase
            {
                Tz = Stockholm,
                Given = new TimeRecord(2019, 1, Mon, 0, 0, 45),
                Expect = (2018, 12, 31)
            },
            ["2019-W36-01"] = new GetGregorianYearCase
            {
                Tz = Stockholm,
                Given = new TimeRecord(2019, 36, Mon, 0, 0, 45),
                Expect = (2019, 9, 2)
            },
            ["2020-W01-01"] = new GetGregorianYearCase
            {
                Tz = Stockholm,
                Given = new TimeRecord(2020, 1, Mon, 0, 0, 45),
                Expect = (2019, 12, 30)
            },
            ["2016-W01-01"] = new GetGregorianYearCase
            {
                Tz = Stockholm,
                Given = new TimeRecord(2016, 1, Mon, 0, 0, 45),
                Expect = (2016, 1, 4)    
            },
            ["2016-W09-01"] = new GetGregorianYearCase
            {
                Tz = Stockholm,
                Given = new TimeRecord(2016, 9, Mon, 0, 0, 45),
                Expect = (2016, 2, 29)
            },
            ["2016-W52-06"] = new GetGregorianYearCase
            {
                Tz = Stockholm,
                Given = new TimeRecord(2016,52, Sat, 0, 0, 45),
                Expect = (2016, 12, 31)
            }
        };

        [TestMethod]
        [DataRow("2019-W01-01")]
        [DataRow("2019-W36-01")]
        [DataRow("2020-W01-01")]
        [DataRow("2016-W01-01")]
        [DataRow("2016-W09-01")]
        [DataRow("2016-W52-06")]
        public void GetGregorianYear(string label)
        {
            var testCase = GetGregorianYearCases[label];
            var e = testCase.Expect;
            var a = testCase.Given.ToGregorian(testCase.Tz);

            Assert.AreEqual(e, a);
        }

        [TestMethod]
        [DataRow("MondayAtTenW35Y2019Oneoff")]
        [DataRow("MondayAtTenW35Y2019ForTen")]
        [DataRow("RecurOverYearEndShortYear")]
        public void GetPosixFromTimeRecord(string label)
        {
            var testCase = FirstlastOccurenceCases[label];
            var tz = testCase.Tz;

            var firstO = testCase.Given.ToPosix(tz);
            var firstE = testCase.Expect.first;

            Assert.AreEqual(firstE, firstO, "First");
        }
    }
}