with Ahven.Text_Runner;
with Ahven.Framework;

with Mission_Tests;

procedure Mission_Check is
   Test_Suite : Ahven.Framework.Test_Suite := Ahven.Framework.Create_Suite ("All");
begin
--     System.Exception_Traces.Set_Trace_Decorator
--       (System.Traceback.Symbolic.Symbolic_Traceback'Access);
   Mission_Tests.Add_Tests (Test_Suite);
   Ahven.Text_Runner.Run (Test_Suite);
end Mission_Check;
