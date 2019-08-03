using System.Threading.Tasks;
using System.Net;
using Microsoft.AspNetCore.Mvc.Testing;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using Sut = PublicCallers;

namespace tests
{
    [TestClass]
    public class EndpointTests
    {
        [AssemblyInitialize]
        public static void AssemblyInit(TestContext context)
        {
            _factory = new WebApplicationFactory<Sut.Startup>();
        }

        private static WebApplicationFactory<Sut.Startup> _factory;
        
        [TestMethod]
        public async Task LoginRedirects()
        {
            var opts = new WebApplicationFactoryClientOptions
            {
                AllowAutoRedirect = false
            };

            var client = _factory.CreateClient(opts);
            var r = await client.GetAsync("/login");

            Assert.IsTrue(r.StatusCode == HttpStatusCode.Redirect);
        }
    }
}
