using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Runtime.InteropServices;

namespace F2x.Glue
{
    /// <summary>
    /// Access to one-dimensional REAL(8) arrays.
    /// </summary>
    internal class F1DArrayDoubleAccess : FArrayAccess<Double>
    {
        // Internal pointer to array memory.
        private IntPtr ptr;

        // Internal storage of size.
        private Int32 size;

        /// <summary>
        /// Create new accessor for given Fortran memory.
        /// </summary>
        /// <param name="ptr">Address of the allocated Fortran memory.</param>
        public F1DArrayDoubleAccess(IntPtr ptr, Int32 size)
        {
            this.ptr = ptr;
            this.size = size;
        }

        /// <see cref="FArrayAccess{T}.GetItem(Int32[])" />
        public Double GetItem(Int32[] index)
        {
            Double[] value = new Double[1];
            Int32 offset = index[0] * Marshal.SizeOf<Double>();
            Marshal.Copy(IntPtr.Add(this.ptr, offset), value, 0, 1);
            return value[0];
        }

        /// <see cref="FArrayAccess{T}.CopyTo(T[])" />
        public void CopyTo(Double[] dest)
        {
            Int32 size = Math.Min(this.size, dest.Length);
            Marshal.Copy(this.ptr, dest, 0, size);
        }

        /// <see cref="FArrayAccess{T}.SetItem(int[], T)" />
        public void SetItem(Int32[] index, Double value)
        {
            Double[] value_arr = new Double[] { value };
            Int32 offset = index[0] * Marshal.SizeOf<Double>();
            Marshal.Copy(value_arr, 0, IntPtr.Add(this.ptr, offset), 1);
        }

        /// <see cref="FArrayAccess{T}.CopyFrom(T[])" />
        public void CopyFrom(Double[] source)
        {
            Int32 size = Math.Min(this.size, source.Length);
            Marshal.Copy(source, 0, this.ptr, size);
        }
    }

    /// <summary>
    /// One-dimensional REAL(8) array contained in a derived type.
    /// </summary>
    public class F1DArrayFieldDouble : FArrayField<Double>
    {
        // Internal pointer to the derived type instance.
        private IntPtr owner;

        // Internal cache for access manager.
        private FArrayAccess<Double> access;

        /// <summary>
        /// Manage an one-dimensinal array that is a field of a derived Fortran type.
        /// </summary>
        /// <param name="owner">Address of the instance containing this array.</param>
        /// <param name="size">Size of the array.</param>
        /// <param name="getter">Retrieve array address from owner.</param>
        public F1DArrayFieldDouble(IntPtr owner, Int32[] size, FTypeGetArray getter) : base(getter(owner), size)
        {
            this.owner = owner;
            this.access = new F1DArrayDoubleAccess(this.ptr, size[0]);
        }

        /// <see cref="FArray{T}.Access" />
        protected override FArrayAccess<Double> Access
        {
            get
            {
                return this.access;
            }
        }

        /// <summary>
        /// Convenient accessor to enable simple indexing.
        /// </summary>
        /// <param name="index">Index of the item to retrieve.</param>
        /// <returns>The Indexed value.</returns>
        public Double this[Int32 index]
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

        /// <summary>
        /// Fortran REAL(8) one-dimensional array.
        /// </summary>
        public class F1DArrayDouble : FStaticArray<Double>
        {
            // Internal cache for access manager.
            private FArrayAccess<Double> access;

            /// <summary>
            /// Allocate a new Fortran REAL(8) Array.
            /// </summary>
            /// <param name="size">Size of the array (only one item).</param>
            public F1DArrayDouble(Int32[] size) : base(size)
            {
                this.access = null;
            }

            /// <summary>
            /// Releases memory.
            /// </summary>
            ~F1DArrayDouble()
            {
                this.Free();
            }

            /// <summary>
            /// Uses CoTaskMem to allocate memory for array.
            /// </summary>
            /// <param name="size">Size of array to allocate.</param>
            /// <returns></returns>
            protected override IntPtr Allocate(int[] size)
            {
                this.access = null;
                return Marshal.AllocCoTaskMem(size[0] * Marshal.SizeOf<Double>());
            }

            /// <summary>
            /// Release CoTaskMem.
            /// </summary>
            protected override void Free()
            {
                this.access = null;
                Marshal.FreeCoTaskMem(this.ptr);
            }

            /// <see cref="FArray{T}.Access" />
            protected override FArrayAccess<Double> Access
            {
                get
                {
                    if (this.access == null)
                    {
                        this.access = new F1DArrayDoubleAccess(this.ptr, this.size[0]);
                    }
                    return this.access;
                }
            }

            /// <summary>
            /// Convenience accessor for one-dimensinal array.
            /// </summary>
            /// <param name="index">Index of the item to get.</param>
            /// <returns>Value at index.</returns>
            public Double this[Int32 index]
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

    /// <summary>
    /// A dynamically allocated field of type REAL(8) contained in a derived type.
    /// </summary>
    public class FDynamicArrayFieldDouble : FDynamicArrayField<Double>
    {
        /// <summary>
        /// Access array that is a field of a derived type instance.
        /// </summary>
        /// <param name="owner">Address of the derived type memory.</param>
        /// <param name="alloc">Allocator for the array field.</param>
        /// <param name="clear">Deallocator for the array field.</param>
        /// <param name="size">Size getter for the array field.</param>
        /// <param name="getter">Getter for array address.</param>
        public FDynamicArrayFieldDouble(IntPtr owner, FTypeAllocArray alloc, FTypeClearArray clear, FTypeArraySize size, FTypeGetArray getter) : base(owner, alloc, clear, size, getter)
        {
        }

        /// <see cref="FArray{T}.Access" />
        protected override FArrayAccess<Double> Access
        {
            get
            {
                if (this.access == null)
                {
                    this.size = new Int32[] { this.ArraySize(this.owner) };
                    this.access = new F1DArrayDoubleAccess(this.GetArray(this.owner), this.size[0]);
                }
                return this.access;
            }
        }
    }
}
