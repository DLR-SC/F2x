using System;

using SOURCE;

class SourceTest {
	static Int32 Main(String[] args) {
		BASIC_TYPE bt = new BASIC_TYPE();
		bt.INTFIELD = 1;
		bt.REALFIELD = 2.3;
		bt.LOGICALFIELD = 0;
//		bt.CHARFIELD = "test";

		for (Int32 i = 0; i < bt.INTARRAY.Size; i++) {
		    bt.INTARRAY[i] = (i + 1) * 2;
		}

		bt.REALARRAY.Init(2);
		bt.REALARRAY[0] = 1.2;
		bt.REALARRAY[1] = 3.4;

		Console.WriteLine("bt.INTFIELD = {0}", bt.INTFIELD);
		Console.WriteLine("bt.REALFIELD = {0}", bt.REALFIELD);
		Console.WriteLine("bt.LOGICALFIELD = {0}", bt.LOGICALFIELD);
//		Console.WriteLine("bt.CHARFIELD = {0}", bt.CHARFIELD);
		Console.Write("bt.INTARRAY = { ");
		foreach (Int32 v in bt.INTARRAY) {
		    Console.Write("{0} ", v);
		}
		Console.WriteLine("}");
		Console.Write("bt.REALARRAY = { ");
		foreach (Double v in bt.REALARRAY) {
		    Console.Write("{0} ", v);
		}
		Console.WriteLine("}");

		COMPOUND_TYPE ct = new COMPOUND_TYPE();
		ct.BASICFIELD.INTFIELD = 2;
		ct.ALLOCATEFIELD.INTFIELD = 3;
		ct.BASICARRAY.Resize(1);

		Console.WriteLine("ct.BASICFIELD.INTFIELD = {0}", ct.BASICFIELD.INTFIELD);
		Console.WriteLine("ct.ALLOCATEFIELD.INTFIELD = {0}", ct.ALLOCATEFIELD.INTFIELD);

        Int32 intval = 0;
        Double realval = 0;
        Int32 logicval = 0;

        SOURCE.SOURCE.BASIC_ARGS_OUT(ref intval, ref realval, ref logicval);
        Console.WriteLine("BASIC_ARGS_OUT() -> {0} {1} {2}", intval, realval, logicval);

        Console.Write("BASIC_ARGS_INOUT({0} {1} {2}) -> ", intval, realval, logicval);
        SOURCE.SOURCE.BASIC_ARGS_INOUT(ref intval, ref realval, ref logicval);
        Console.WriteLine("{0} {1} {2}", intval, realval, logicval);

        Console.WriteLine("BASIC_ARGS_IN({0} {1} {2})", intval, realval, logicval);
        SOURCE.SOURCE.BASIC_ARGS_IN(intval, realval, logicval);

        Int32[] inarray = new Int32[] { 1, 2, 3 };
        Int32[] outarray = new Int32[3];
        Int32[] inoutarray = new Int32[] { 5, 6, 7 };
        SOURCE.SOURCE.BASIC_ARGS_ARRAY(inarray, ref outarray, ref inoutarray);
        Console.Write("BASIC_ARGS_ARRAY(...) -> { ");
        foreach (Int32 i in outarray) {
            Console.Write("{0} ", i);
        }
        Console.Write("} { ");
        foreach (Int32 i in inoutarray) {
            Console.Write("{0} ", i);
        }
        Console.WriteLine("}");

        BASIC_TYPE inarg = new BASIC_TYPE();
        BASIC_TYPE outarg = new BASIC_TYPE(IntPtr.Zero, false);
        BASIC_TYPE inoutarg = new BASIC_TYPE();

        inarg.INTFIELD = 3;
        inoutarg.REALFIELD = 5.6;

        SOURCE.SOURCE.DERIVED_TYPE_ARGS(inarg, ref outarg, ref inoutarg);
        Console.WriteLine("DERIVED_TYPE_ARGS(...) -> {0} {1}", outarg.REALFIELD, inoutarg.INTFIELD);

        String instr = "in";
        String outstr = "out";
        String inoutstr = "inout";
//        SOURCE.SOURCE.STRING_ARGS(instr, ref outstr, ref inoutstr);
        Console.WriteLine("STRING_ARGS(...) -> {0} {1}", outstr, inoutstr);

        Console.WriteLine("BASIC_RETURN_VALUE() = {0}", SOURCE.SOURCE.BASIC_RETURN_VALUE());
        Console.WriteLine("DERIVED_TYPE_RETURN_VALUE() = {0}", SOURCE.SOURCE.DERIVED_TYPE_RETURN_VALUE());
//        Console.WriteLine("STRING_RETURN_VALUE() = {0}", SOURCE.SOURCE.STRING_RETURN_VALUE());

        Int32[] arrayres = SOURCE.SOURCE.ARRAY_RETURN_VALUE();
        Console.Write("ARRAY_RETURN_VALUE() = { ");
        foreach (Int32 i in arrayres) {
            Console.Write("{0} ", i);
        }
        Console.WriteLine("}");

		return 0;
	}
}

