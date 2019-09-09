using System;
using System.Linq;

namespace timeutil
{
    class Program
    {
        static void Main(string[] args)
        {
            Console.WriteLine("timezone");
            Console.WriteLine(string.Join(
                    Environment.NewLine, 
                    from z in TimeZoneInfo.GetSystemTimeZones()
                    select z.Id));
        }
    }
}
