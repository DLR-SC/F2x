namespace F2x.Glue {
	using System;
	using System.Collections;
	using System.Collections.Generic;
	using System.Runtime.InteropServices;

	public abstract class DynamicArray<T> : F2Array<T> {
		protected InitFunc InitArray;
		protected ClearFunc ClearArray;
		protected GetSizeFunc GetSize;

		public DynamicArray() {
		}

		public DynamicArray(IntPtr ptr, InitFunc init, ClearFunc clear, GetSizeFunc size, GetAddrFunc getaddr) {
			this.ptr = ptr;
			this.InitArray = init;
			this.ClearArray = clear;
			this.GetSize = size;
			this.GetAddr = getaddr;
		}

		public override Int32 Count {
			get {
				return this.GetSize(this.ptr);
			}
		}

		public override void Clear() {
			this.ClearArray(this.ptr);
		}

		public void Resize(Int32 size) {
			if (this.Count > 0) {
				this.Clear();
			}

			this.InitArray(this.ptr, size);
		}
	}

	public class DynamicInt32Array : DynamicArray<Int32> {
		public DynamicInt32Array(IntPtr ptr, InitFunc init, ClearFunc clear, GetSizeFunc size, GetAddrFunc getaddr)
		: base(ptr, init, clear, size, getaddr) {
		}

		public override Int32 GetItem(Int32 index) {
			IntPtr addr = this.GetAddr(this.ptr);
			Int32[] data = new Int32[1];
			Marshal.Copy(IntPtr.Add(addr, index * sizeof(Int32)), data, 0, 1);
			return data[0];
		}

		public override void SetItem(Int32 index, Int32 value) {
			IntPtr addr = this.GetAddr(this.ptr);
			Int32[] data = new Int32[] { value };
			Marshal.Copy(data, 0, IntPtr.Add(addr, index * sizeof(Int32)), 1);
		}

		public override void CopyTo(Int32[] array, Int32 arrayIndex) {
			IntPtr addr = this.GetAddr(this.ptr);
			Marshal.Copy(addr, array, arrayIndex, this.Count);
		}

		public override void CopyFrom(Int32[] array, Int32 arrayIndex) {
			IntPtr addr = this.GetAddr(this.ptr);
			Marshal.Copy(array, arrayIndex, addr, this.Count);
		}
	}

	public class DynamicDoubleArray : DynamicArray<Double> {
		public DynamicDoubleArray(IntPtr ptr, InitFunc init, ClearFunc clear, GetSizeFunc size, GetAddrFunc getaddr)
		: base(ptr, init, clear, size, getaddr) {
		}

		public override Double GetItem(Int32 index) {
			IntPtr addr = this.GetAddr(this.ptr);
			Double[] data = new Double[1];
			Marshal.Copy(IntPtr.Add(addr, index * sizeof(Double)), data, 0, 1);
			return data[0];
		}

		public override void SetItem(Int32 index, Double value) {
			IntPtr addr = this.GetAddr(this.ptr);
			Double[] data = new Double[] { value };
			Marshal.Copy(data, 0, IntPtr.Add(addr, index * sizeof(Double)), 1);
		}

		public override void CopyTo(Double[] array, Int32 arrayIndex) {
			IntPtr addr = this.GetAddr(this.ptr);
			Marshal.Copy(addr, array, arrayIndex, this.Count);
		}

		public override void CopyFrom(Double[] array, Int32 arrayIndex) {
			IntPtr addr = this.GetAddr(this.ptr);
			Marshal.Copy(array, arrayIndex, addr, this.Count);
		}
	}
}

