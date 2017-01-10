<<<<<<< Updated upstream
﻿using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Runtime.InteropServices;

namespace F2x.Glue
{
    internal class F1DArrayInt32Access : FArrayAccess<Int32>
    {
        private IntPtr ptr;
        private Int32 size;

        public F1DArrayInt32Access(IntPtr ptr, Int32 size)
        {
            this.ptr = ptr;
            this.size = size;
        }

        public Int32 GetItem(Int32[] index)
        {
            Int32[] value = new Int32[1];
            Int32 offset = index[0] * Marshal.SizeOf<Int32>();
            Marshal.Copy(IntPtr.Add(this.ptr, offset), value, 0, 1);
            return value[0];
        }

        public void CopyTo(Int32[] dest)
        {
            Int32 size = Math.Min(this.size, dest.Length);
            Marshal.Copy(this.ptr, dest, 0, size);
        }

        public void SetItem(Int32[] index, Int32 value)
        {
            Int32[] value_arr = new Int32[] { value };
            Int32 offset = index[0] * Marshal.SizeOf<Int32>();
            Marshal.Copy(value_arr, 0, IntPtr.Add(this.ptr, offset), 1);
        }

        public void CopyFrom(Int32[] source)
        {
            Int32 size = Math.Min(this.size, source.Length);
            Marshal.Copy(source, 0, this.ptr, size);
        }
    }

    public class F1DArrayFieldInt32 : FArrayField<Int32>
    {
        private IntPtr owner;
        private FArrayAccess<Int32> access;

        public F1DArrayFieldInt32(IntPtr owner, Int32[] size, FTypeGetArray getter) : base(getter(owner), size)
        {
            this.owner = owner;
            this.access = new F1DArrayInt32Access(this.ptr, size[0]);
        }

        protected override FArrayAccess<Int32> Access
        {
            get
            {
                return this.access;
            }
        }

        public Int32 this[Int32 index]
        {
            get
            {
                return this[new Int32[] { index }];
            }
            set
            {
                this[new Int32[] { index }] = value;
            }
        }

        public class F1DArrayInt32 : FStaticArray<Int32>
        {
            private FArrayAccess<Int32> access;

            public F1DArrayInt32(Int32[] size) : base(size)
            {
                this.access = null;
            }

            ~F1DArrayInt32()
            {

            }

            protected override IntPtr Allocate(Int32[] size)
            {
                this.access = null;
                return Marshal.AllocCoTaskMem(size[0] * Marshal.SizeOf<Int32>());
            }

            protected override void Free()
            {
                this.access = null;
                Marshal.FreeCoTaskMem(this.ptr);
            }

            protected override FArrayAccess<Int32> Access
            {
                get
                {
                    if (this.access == null)
                    {
                        this.access = new F1DArrayInt32Access(this.ptr, this.size[0]);
                    }
                    return this.access;
                }
            }

            public Int32 this[Int32 index]
            {
                get
                {
                    return this[new Int32[] { index }];
                }
                set
                {
                    this[new Int32[] { index }] = value;
                }
            }
        }
    }

    public class FDynamicArrayFieldInt32 : FDynamicArrayField<Int32>
    {
        public FDynamicArrayFieldInt32(IntPtr owner, FTypeAllocArray alloc, FTypeClearArray clear, FTypeArraySize size, FTypeGetArray getter) : base(owner, alloc, clear, size, getter)
        {
        }

        protected override FArrayAccess<Int32> Access
        {
            get
            {
                if (this.access == null)
                {
                    this.size = new Int32[] { this.ArraySize(this.owner) };
                    this.access = new F1DArrayInt32Access(this.GetArray(this.owner), this.size[0]);
                }
                return this.access;
            }
        }
    }
}
=======
﻿using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Runtime.InteropServices;

namespace F2x.Glue
{
    internal class F1DArrayInt32Access : FArrayAccess<Int32>
    {
        private IntPtr ptr;
        private Int32 size;

        public F1DArrayInt32Access(IntPtr ptr, Int32 size)
        {
            this.ptr = ptr;
            this.size = size;
        }

        public Int32 GetItem(Int32[] index)
        {
            Int32[] value = new Int32[1];
            Int32 offset = index[0] * Marshal.SizeOf<Int32>();
            Marshal.Copy(IntPtr.Add(this.ptr, offset), value, 0, 1);
            return value[0];
        }

        public void CopyTo(Int32[] dest)
        {
            Int32 size = Math.Min(this.size, dest.Length);
            Marshal.Copy(this.ptr, dest, 0, size);
        }

        public void SetItem(Int32[] index, Int32 value)
        {
            Int32[] value_arr = new Int32[] { value };
            Int32 offset = index[0] * Marshal.SizeOf<Int32>();
            Marshal.Copy(value_arr, 0, IntPtr.Add(this.ptr, offset), 1);
        }

        public void CopyFrom(Int32[] source)
        {
            Int32 size = Math.Min(this.size, source.Length);
            Marshal.Copy(source, 0, this.ptr, size);
        }
    }

    public class F1DArrayFieldInt32 : FArrayField<Int32>
    {
        private IntPtr owner;
        private FArrayAccess<Int32> access;

        public F1DArrayFieldInt32(IntPtr owner, Int32[] size, FTypeGetArray getter) : base(getter(owner), size)
        {
            this.owner = owner;
            this.access = new F1DArrayInt32Access(this.ptr, size[0]);
        }

        protected override FArrayAccess<Int32> Access
        {
            get
            {
                return this.access;
            }
        }

        public Int32 this[Int32 index]
        {
            get
            {
                return this[new Int32[] { index }];
            }
            set
            {
                this[new Int32[] { index }] = value;
            }
        }

        public class F1DArrayInt32 : FStaticArray<Int32>
        {
            private FArrayAccess<Int32> access;

            public F1DArrayInt32(Int32[] size) : base(size)
            {
                this.access = null;
            }

            ~F1DArrayInt32()
            {

            }

            protected override IntPtr Allocate(Int32[] size)
            {
                this.access = null;
                return Marshal.AllocCoTaskMem(size[0] * Marshal.SizeOf<Int32>());
            }

            protected override void Free()
            {
                this.access = null;
                Marshal.FreeCoTaskMem(this.ptr);
            }

            protected override FArrayAccess<Int32> Access
            {
                get
                {
                    if (this.access == null)
                    {
                        this.access = new F1DArrayInt32Access(this.ptr, this.size[0]);
                    }
                    return this.access;
                }
            }

            public Int32 this[Int32 index]
            {
                get
                {
                    return this[new Int32[] { index }];
                }
                set
                {
                    this[new Int32[] { index }] = value;
                }
            }
        }
    }

    public class FDynamicArrayFieldInt32 : FDynamicArrayField<Int32>
    {
        public FDynamicArrayFieldInt32(IntPtr owner, FTypeAllocArray alloc, FTypeClearArray clear, FTypeArraySize size, FTypeGetArray getter) : base(owner, alloc, clear, size, getter)
        {
        }

        protected override FArrayAccess<Int32> Access
        {
            get
            {
                if (this.access == null)
                {
                    this.size = new Int32[] { this.ArraySize(this.owner) };
                    this.access = new F1DArrayInt32Access(this.GetArray(this.owner), this.size[0]);
                }
                return this.access;
            }
        }
    }
}
>>>>>>> Stashed changes
