using System;
using Xunit;
using Xunit.Abstractions;

using CPN.Simulator.Domain;
using System.Linq;

namespace CPN.Simulator.Tests.CSharp
{
    public class RuntimeTest
    {
        private readonly ITestOutputHelper output;

        public RuntimeTest(ITestOutputHelper output)
        {
            this.output = output;
        }
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

            // Given that F# is more succint and C# asserting of this data types
            // is really verbose, we could also check converting it to a string
            // for a shorter version.
            Assert.Equal("[(P 2, 1`()); (P 4, 3`()); (P 5, 3`())]",
                         lastStateMarking.ToString());
        }

        [Fact]
        public void RuntimeStepTest()
        {
            // A way to use the simulator could be step by step:
            var unfinished = true;
            var cpn = SampleNets.notSoSimpleNet;
            var completeMarking = "";

            while (unfinished) {
                // Later we'll compare the net marking with the allSteps form,
                // so we need to save the state.
                completeMarking += CPNModule.netMarking(cpn).ToString() + "\n";
                
                switch (Runtime.step(cpn))
                {
                    case var checkResult when checkResult.IsOk:
                        (unfinished, cpn) = checkResult.ResultValue;
                        break;
                    case var checkResult when checkResult.IsError:
                        (unfinished, cpn) = (false, null);
                        break;
                }
            }
            
            // The net should end at the same spot as the last test
            Assert.Equal("[(P 2, 1`()); (P 4, 3`()); (P 5, 3`())]",
                         CPNModule.netMarking(cpn).ToString());

            // The marking generated through all steps should be equal, simulating
            // them one by one or all at the same time.
            var completeMarking2 = 
                Runtime.allSteps(SampleNets.notSoSimpleNet)
                       .Select(cpn => CPNModule.netMarking(cpn).ToString())
                       .Aggregate((acc, strMarking) => acc + "\n" + strMarking) + "\n";

            Assert.Equal(completeMarking, completeMarking2);
        }
    }
}
