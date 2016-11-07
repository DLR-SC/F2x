namespace F2x.Glue {
	using System;
	using System.Collections;
	using System.Collections.Generic;

	public abstract class F2Array<T> : IList<T> {
		protected IntPtr ptr = IntPtr.Zero;
		protected GetAddrFunc GetAddr;

		public F2Array() {
		}

		public abstract T GetItem(Int32 index);
		public abstract void SetItem(Int32 index, T value);

		public abstract Int32 Count { get; }

		public Boolean IsReadOnly {
			get {
				return false;
			}
		}

		public T this[Int32 index] {
			get {
				if (index < 0 || index >= this.Count) {
					throw new IndexOutOfRangeException();
				}
				return this.GetItem(index);
			}
			set {
				if (index < 0 || index >= this.Count) {
					throw new IndexOutOfRangeException();
				}
				this.SetItem(index, value);
			}
		}

		public Int32 IndexOf(T value) {
			return -1;
		}

		public void Insert(Int32 index, T value) {	
		}

		public void RemoveAt(Int32 index) {
		}

		public void Add(T value) {
		}

		public Boolean Remove(T value) {
			return false;
		}
		
		public virtual void Clear() {
		}

		public Boolean Contains(T value) {
			return false;
		}

		public abstract void CopyTo(T[] array, Int32 arrayIndex);

		public abstract void CopyFrom(T[] array, Int32 arrayIndex);

		IEnumerator IEnumerable.GetEnumerator() {
			return this.GetEnumerator();
		}

		public IEnumerator<T> GetEnumerator() {
			T[] array = new T[this.Count];
			this.CopyTo(array, 0);
			foreach (T item in array) {
				yield return item;
			}
		}
	}
}

