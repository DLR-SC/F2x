<<<<<<< Updated upstream
﻿using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace F2x.Glue
{
    /// <summary>
    /// Get address of an array contained in a derived type.
    /// </summary>
    /// <param name="ptr">Address of the derived type instance.</param>
    /// <returns>Address of the array in the instance.</returns>
    public delegate IntPtr FTypeGetArray(IntPtr ptr);

    public delegate void FTypeAllocArray(IntPtr ptr, Int32 size);
    public delegate void FTypeClearArray(IntPtr ptr);
    public delegate Int32 FTypeArraySize(IntPtr ptr);

    public abstract class FArrayField<T> : FArray<T>
    {
        /// <summary>
        /// The pointer to the Fortran array.
        /// </summary>
        protected IntPtr ptr;

        /// <summary>
        /// The size of this array for each dimension.
        /// </summary>
        protected Int32[] size;

        /// <summary>
        /// Access an array at the given Fortran address with the given sizes.
        /// </summary>
        /// <param name="ptr">Address of Fortran array memory.</param>
        /// <param name="size">Sizes per dimension.</param>
        protected FArrayField(IntPtr ptr, Int32[] size)
        {
            this.ptr = ptr;
            this.size = size;
        }

        /// <summary>
        /// Retrieve the Fortran pointer for the array.
        /// </summary>
        public IntPtr Ptr
        {
            get
            {
                return this.ptr;
            }
        }

        /// <see cref="FArray{T}.Count" />
        public override Int32[] Count
        {
            get
            {
                return this.size;
            }
        }
    }

    /// <summary>
    /// This extends the static array field of a derived type to be allocatable.
    /// </summary>
    /// <typeparam name="T"></typeparam>
    public abstract class FDynamicArrayField<T> : FStaticArray<T>
    {
        // Internal cache of access manager.
        protected FArrayAccess<T> access;

        // Internal pointer to owner memory (i.e. derived type instance).
        protected IntPtr owner;

        // Allocate memory.
        protected FTypeAllocArray AllocArray;

        // Clear memory, discard data.
        protected FTypeClearArray ClearArray;

        // Get current allocation size.
        protected FTypeArraySize ArraySize;

        // Get address of array memory.
        protected FTypeGetArray GetArray;

        /// <summary>
        /// Create a new manager for an array field in a derived type.
        /// </summary>
        /// <param name="owner">Address of derived type instance memory.</param>
        /// <param name="alloc">Allocator for the field.</param>
        /// <param name="clear">Deallocator for the field.</param>
        /// <param name="getsize">Size query for the field.</param>
        /// <param name="getter">Getter for array address.</param>
        protected FDynamicArrayField(IntPtr owner, FTypeAllocArray alloc, FTypeClearArray clear, FTypeArraySize getsize, FTypeGetArray getter) : base(IntPtr.Zero, new Int32[] { 0 })
        {
            this.access = null;
            this.owner = owner;
            this.AllocArray = alloc;
            this.ClearArray = clear;
            this.ArraySize = getsize;
            this.GetArray = getter;
        }

        /// <summary>
        /// Initialize the array field with the given size. Old data will be dropped.
        /// </summary>
        /// <param name="size">New size of the array.</param>
        public void Init(Int32 size)
        {
            if (this.ptr != IntPtr.Zero)
            {
                this.Free();
            }

            this.ptr = this.Allocate(new Int32[] { size });
            this.size = new Int32[] { this.ArraySize(this.owner) };
        }

        /// <see cref="FStaticArray{T}.Allocate(Int32[])" />
        override protected IntPtr Allocate(Int32[] size)
        {
            this.access = null;
            this.AllocArray(this.owner, size[0]);
            return this.GetArray(this.owner);
        }

        /// <see cref="FStaticArray{T}.Free" />
        override protected void Free()
        {
            this.access = null;
            this.ClearArray(this.owner);
        }

        public T this[Int32 index]
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
=======
﻿using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace F2x.Glue
{
    /// <summary>
    /// Get address of an array contained in a derived type.
    /// </summary>
    /// <param name="ptr">Address of the derived type instance.</param>
    /// <returns>Address of the array in the instance.</returns>
    public delegate IntPtr FTypeGetArray(IntPtr ptr);

    public delegate void FTypeAllocArray(IntPtr ptr, Int32 size);
    public delegate void FTypeClearArray(IntPtr ptr);
    public delegate Int32 FTypeArraySize(IntPtr ptr);

    public abstract class FArrayField<T> : FArray<T>
    {
        /// <summary>
        /// The pointer to the Fortran array.
        /// </summary>
        protected IntPtr ptr;

        /// <summary>
        /// The size of this array for each dimension.
        /// </summary>
        protected Int32[] size;

        /// <summary>
        /// Access an array at the given Fortran address with the given sizes.
        /// </summary>
        /// <param name="ptr">Address of Fortran array memory.</param>
        /// <param name="size">Sizes per dimension.</param>
        protected FArrayField(IntPtr ptr, Int32[] size)
        {
            this.ptr = ptr;
            this.size = size;
        }

        /// <summary>
        /// Retrieve the Fortran pointer for the array.
        /// </summary>
        public IntPtr Ptr
        {
            get
            {
                return this.ptr;
            }
        }

        /// <see cref="FArray{T}.Count" />
        public override Int32[] Count
        {
            get
            {
                return this.size;
            }
        }
    }

    /// <summary>
    /// This extends the static array field of a derived type to be allocatable.
    /// </summary>
    /// <typeparam name="T"></typeparam>
    public abstract class FDynamicArrayField<T> : FStaticArray<T>
    {
        // Internal cache of access manager.
        protected FArrayAccess<T> access;

        // Internal pointer to owner memory (i.e. derived type instance).
        protected IntPtr owner;

        // Allocate memory.
        protected FTypeAllocArray AllocArray;

        // Clear memory, discard data.
        protected FTypeClearArray ClearArray;

        // Get current allocation size.
        protected FTypeArraySize ArraySize;

        // Get address of array memory.
        protected FTypeGetArray GetArray;

        /// <summary>
        /// Create a new manager for an array field in a derived type.
        /// </summary>
        /// <param name="owner">Address of derived type instance memory.</param>
        /// <param name="alloc">Allocator for the field.</param>
        /// <param name="clear">Deallocator for the field.</param>
        /// <param name="getsize">Size query for the field.</param>
        /// <param name="getter">Getter for array address.</param>
        protected FDynamicArrayField(IntPtr owner, FTypeAllocArray alloc, FTypeClearArray clear, FTypeArraySize getsize, FTypeGetArray getter) : base(IntPtr.Zero, new Int32[] { 0 })
        {
            this.access = null;
            this.owner = owner;
            this.AllocArray = alloc;
            this.ClearArray = clear;
            this.ArraySize = getsize;
            this.GetArray = getter;
        }

        /// <summary>
        /// Initialize the array field with the given size. Old data will be dropped.
        /// </summary>
        /// <param name="size">New size of the array.</param>
        public void Init(Int32 size)
        {
            if (this.ptr != IntPtr.Zero)
            {
                this.Free();
            }

            this.ptr = this.Allocate(new Int32[] { size });
            this.size = new Int32[] { this.ArraySize(this.owner) };
        }

        /// <see cref="FStaticArray{T}.Allocate(Int32[])" />
        override protected IntPtr Allocate(Int32[] size)
        {
            this.access = null;
            this.AllocArray(this.owner, size[0]);
            return this.GetArray(this.owner);
        }

        /// <see cref="FStaticArray{T}.Free" />
        override protected void Free()
        {
            this.access = null;
            this.ClearArray(this.owner);
        }

        public T this[Int32 index]
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
>>>>>>> Stashed changes
