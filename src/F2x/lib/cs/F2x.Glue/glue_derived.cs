namespace F2x.Glue {
	using System;

	public abstract class F2Type {
        protected IntPtr ptr = IntPtr.Zero;
        protected Boolean owned;
		
		public F2Type() {
			this.owned = false;
		}

		public F2Type(IntPtr ptr, Boolean owned) {
			this.ptr = ptr;
			this.owned = owned;
		}

        public IntPtr Ptr {
            get {
                return this.ptr;
            }
        }

		public void UpdatePtr(IntPtr ptr) {
			this.ptr = ptr;
		}
		
		public abstract void CopyFrom(F2Type other);
	}

	public class DerivedTypeArray<T> : DynamicArray<T> where T : F2Type, new() {
		private GetItemFunc Get;

		public DerivedTypeArray(IntPtr ptr, InitFunc init, ClearFunc clear, GetSizeFunc size, GetItemFunc getitem)
		: base(ptr, init, clear, size, null) {
			this.Get = getitem;
		}

		public override T GetItem(Int32 index) {
			T item = new T();
			IntPtr addr = this.Get(this.ptr, index);
			item.UpdatePtr(addr);
			return item;
		}

		public override void SetItem(Int32 index, T value) {
			T item = this[index];
			item.CopyFrom(value);
		}

		public override void CopyTo(T[] array, Int32 arrayIndex) {
			T item = new T();

			for (Int32 i = 0; i < this.Count; i++) {
				IntPtr addr = this.Get(this.ptr, i);
				item.UpdatePtr(addr);
				array[arrayIndex + i].CopyFrom(item);
			}
		}

		public override void CopyFrom(T[] array, Int32 arrayIndex) {
			T item = new T();

			for (Int32 i = 0; i < this.Count; i++) {
				IntPtr addr = this.Get(this.ptr, i);
				item.UpdatePtr(addr);
				item.CopyFrom(array[arrayIndex + i]);
			}
		}
	}
}

