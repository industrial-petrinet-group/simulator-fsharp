using System;
using Xunit;

using CPN.Simulator.Domain;
using System.Linq;

namespace CPN.Simulator.Tests.CSharp
{
    public class RuntimeTest
    {
        [Fact]
        public void NotSoSimpleNetTest()
        {
            // The notSoSimpleNet has 5 steps until finishing
            var allSteps = Runtime.allSteps(SampleNets.notSoSimpleNet);

            Assert.Equal(5, allSteps.Count());

            // The last state of the net should be of 3 places marked, p2 with 
            // 1`() and p4 & p5 with 3`()
            var lastState = allSteps.ElementAt(allSteps.Count() - 1);
            var lastStateMarking = CPNModule.netMarking(lastState);

            Assert.Equal(3, lastStateMarking.Count());

            // The marking of p2, p4 and p5 is emulated here
            var p2 = new Tuple<PlaceId, string>(PlaceId.NewP(2), "1`()");
            var p4 = new Tuple<PlaceId, string>(PlaceId.NewP(4), "3`()");
            var p5 = new Tuple<PlaceId, string>(PlaceId.NewP(5), "3`()");

            Assert.Equal(p2, lastStateMarking.ElementAt(0));
            Assert.Equal(p4, lastStateMarking.ElementAt(1));
            Assert.Equal(p5, lastStateMarking.ElementAt(2));
        }
    }
}
