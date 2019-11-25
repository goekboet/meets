using System;
using System.IO;
using System.Net;
using System.Net.Http;
using System.Net.Http.Headers;
using System.Text;
using System.Threading.Tasks;
using Microsoft.Extensions.Configuration;
using Microsoft.Extensions.DependencyInjection;
using Microsoft.Extensions.Logging;
using Microsoft.Extensions.Options;

namespace Meets.Services
{
    public static class ServiceExtenstions
    {
        public static IServiceCollection AddBrokerClient(
            this IServiceCollection s,
            IConfiguration c)
        {
            var section = c.GetSection("Broker");
            s.Configure<Broker>(section);
            var o = section.Get<Broker>();
            if (o.Mock)
            {
                s.AddHttpClient<IBookings, Mock>();
            }
            else
            {
                s.AddHttpClient<IBookings, Outbound>();
            }

            return s;
        }
    }

    public class Broker
    {
        public bool Mock { get; set; }
        public string Backend { get; set; }
        public string Frontend { get; set; }
    }

    public class MockedHttpContent : HttpContent
    {
        MemoryStream S { get; }
        public MockedHttpContent(string s)
        {
            S = new MemoryStream(Encoding.UTF8.GetBytes(s));
        }

        protected override Task SerializeToStreamAsync(
            Stream stream,
            TransportContext context)
        {
            return S.CopyToAsync(stream);
        }

        protected override bool TryComputeLength(
            out long length)
        {
            length = 0;
            return true;
        }
    }

    public interface IBookings
    {
        Task<HttpResponseMessage> Post(HttpRequestMessage r);
        Task<HttpResponseMessage> List(HttpRequestMessage r);
        Task<HttpResponseMessage> Delete(HttpRequestMessage r);
    }

    public class Outbound : IBookings
    {
        private HttpClient Client { get; }
        public Outbound(
            IOptions<Broker> opts,
            HttpClient c
        )
        {
            c.BaseAddress = new Uri(opts.Value.Backend);
            c.DefaultRequestHeaders.Accept.Add(
                    new MediaTypeWithQualityHeaderValue("application/json"));

            Client = c;
        }

        public Task<HttpResponseMessage> Delete(
            HttpRequestMessage r) => Client.SendAsync(r);

        public Task<HttpResponseMessage> List(
            HttpRequestMessage r) => Client.SendAsync(r);

        public Task<HttpResponseMessage> Post(
            HttpRequestMessage r) => Client.SendAsync(r);
    }

    public class Mock : IBookings
    {
        private ILogger<Mock> Logger { get; }
        public Mock(
            HttpClient c,
            ILogger<Mock> logger)
        {
            Logger = logger;
        }
        public Task<HttpResponseMessage> Delete(
            HttpRequestMessage r)
        {
            Logger.LogInformation("Mocked call to delte booking");
            return Task.FromResult(
                new HttpResponseMessage
                {
                    StatusCode = HttpStatusCode.OK
                });
        }


        public Task<HttpResponseMessage> List(
            HttpRequestMessage r)
        {
            Logger.LogInformation("Mocked call to list bookings.");

            return Task.FromResult(
                    new HttpResponseMessage
                    {
                        StatusCode = HttpStatusCode.OK,
                        Content = new MockedHttpContent("[]")
                    }
                );
        }


        public Task<HttpResponseMessage> Post(
            HttpRequestMessage r)
        {
            Logger.LogInformation("Mocked call to add booking");

            return Task.FromResult(
                new HttpResponseMessage
                {
                    StatusCode = HttpStatusCode.Created
                });
        }

    }
}