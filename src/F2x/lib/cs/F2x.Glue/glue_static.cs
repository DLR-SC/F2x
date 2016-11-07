namespace F2x.Glue {
	using System;
	using System.Runtime.InteropServices;

	public abstract class StaticArray<T> : F2Array<T> {
		private Int32 count;
		protected IntPtr addr;
		
		public StaticArray() {
		}

		public StaticArray(IntPtr ptr, Int32 count, GetAddrFunc getaddr) {
			this.count = count;
			this.addr = getaddr(ptr);
		}
		
		public override Int32 Count {
			get {
				return this.count;
			}
		}
	}

	public class StaticInt32Array : StaticArray<Int32> {
		public StaticInt32Array(IntPtr ptr, Int32 count, GetAddrFunc getaddr) : base(ptr, count, getaddr) {
		}

		public override Int32 GetItem(Int32 index) {
			Int32[] data = new Int32[1];
			Marshal.Copy(IntPtr.Add(this.addr, index * sizeof(Int32)), data, 0, 1);
			return data[0];
		}

		public override void SetItem(Int32 index, Int32 value) {
			Int32[] data = new Int32[] { value };
			Marshal.Copy(data, 0, IntPtr.Add(this.addr, index * sizeof(Int32)), 1);
		}

		public override void CopyTo(Int32[] array, Int32 arrayIndex) {
			Marshal.Copy(this.addr, array, arrayIndex, this.Count);
		}

		public override void CopyFrom(Int32[] array, Int32 arrayIndex) {
			Marshal.Copy(array, arrayIndex, this.addr, this.Count);
		}
	}

	public class StaticDoubleArray : StaticArray<Double> {
		public StaticDoubleArray(IntPtr ptr, Int32 count, GetAddrFunc getaddr) : base(ptr, count, getaddr) {
		}
		
		public override Double GetItem(Int32 index) {
			Double[] data = new Double[1];
			Marshal.Copy(IntPtr.Add(this.addr, index * sizeof(Double)), data, 0, 1);
			return data[0];
		}

		public override void SetItem(Int32 index, Double value) {
			Double[] data = new Double[] { value };
			Marshal.Copy(data, 0, IntPtr.Add(this.addr, index * sizeof(Double)), 1);
		}

		public override void CopyTo(Double[] array, Int32 arrayIndex) {
			Marshal.Copy(this.addr, array, arrayIndex, this.Count);
		}

		public override void CopyFrom(Double[] array, Int32 arrayIndex) {
			Marshal.Copy(array, arrayIndex, this.addr, this.Count);
		}
	}
}

