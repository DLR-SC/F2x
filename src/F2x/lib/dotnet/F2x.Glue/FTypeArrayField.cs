using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace F2x.Glue
{
    public delegate IntPtr FTypeGetArrayItem(IntPtr ptr, Int32 index);

    internal class FTypeArrayFieldAccess<T> : FArrayAccess<T> where T : FType, new()
    {
        private IntPtr owner;
        private FTypeArraySize ArraySize;
        private FTypeGetArrayItem GetArrayItem;

        public FTypeArrayFieldAccess(IntPtr owner, FTypeArraySize size, FTypeGetArrayItem getter)
        {
            this.owner = owner;
            this.ArraySize = size;
            this.GetArrayItem = getter;
        }

        public T GetItem(Int32[] index)
        {
            T item = new T();
            item.UpdatePtr(this.GetArrayItem(this.owner, index[0]));
            return item;
        }

        public void CopyTo(T[] dest)
        {
            Int32 size = Math.Min(this.ArraySize(this.owner), dest.Length);
            for (Int32 i = 0; i < size; i++)
            {
                dest[i].CopyFrom(this.GetItem(new Int32[] { i }));
            }
        }

        public void SetItem(Int32[] index, T value)
        {
            T item = this.GetItem(index);
            item.CopyFrom(value);
        }

        public void CopyFrom(T[] source)
        {
            Int32 size = Math.Min(this.ArraySize(this.owner), source.Length);
            for (Int32 i = 0; i < size; i++)
            {
                this.SetItem(new Int32[] { i }, source[i]);
            }
        }
    }

    public class FTypeArrayField<T> : FDynamicArrayField<T> where T : FType, new()
    {
        public FTypeArrayField(IntPtr owner, FTypeAllocArray alloc, FTypeClearArray clear, FTypeArraySize getsize, FTypeGetArrayItem getter) : base(owner, alloc, clear, getsize, null)
        {
            this.access = new FTypeArrayFieldAccess<T>(owner, getsize, getter);
        }

        protected override FArrayAccess<T> Access
        {
            get
            {
                return this.access;
            }
        }
    }
}
