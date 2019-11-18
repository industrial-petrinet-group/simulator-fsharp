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
            var result = Runtime.allSteps(SampleNets.notSoSimpleNet);

            Assert.Equal(5, result.Count());

            var lastMarking = CPNModule.netMarking(result.ElementAt(4));

            Assert.Equal(3, lastMarking.Count());

            Assert.Equal("1`()", lastMarking.ElementAt(0).Item2);
            Assert.Equal("3`()", lastMarking.ElementAt(1).Item2);
            Assert.Equal("3`()", lastMarking.ElementAt(2).Item2);
        }
    }
}
