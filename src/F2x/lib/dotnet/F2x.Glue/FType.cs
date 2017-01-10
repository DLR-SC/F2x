<<<<<<< Updated upstream
﻿using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace F2x.Glue
{
    /// <summary>
    /// Allocate new memory in Fortran.
    /// </summary>
    /// <returns>Address of the allocated memory.</returns>
    public delegate IntPtr FTypeNew();

    /// <summary>
    /// Release memory allocated from Fortran.
    /// </summary>
    /// <param name="ptr">Address of memory to free.</param>
    public delegate void FTypeFree(IntPtr ptr);

    /// <summary>
    /// Basic type for all Fortran derived types.
    /// </summary>
    public abstract class FType
    {
        // Internal pointer to allocated memory.
        protected IntPtr ptr;

        // Flag indicating whether the memory is owned (i.e. may be freed upon disposal).
        protected Boolean owned;

        /// <summary>
        /// Get access to the plain memory.
        /// </summary>
        public IntPtr Ptr
        {
            get
            {
                return this.ptr;
            }
        }

        /// <summary>
        /// Associate a plain pointer with this type.
        /// </summary>
        /// <param name="ptr">Address of the Fortran memory.</param>
        /// <param name="owned">Indicates whether the memory is owned, i.e. will be freed by the destructor.</param>
        protected FType(IntPtr ptr, Boolean owned)
        {
            this.ptr = ptr;
            this.owned = owned;
        }

        /// <summary>
        /// Copy the data field from an identic type.
        /// </summary>
        /// <param name="other">Instance of the type.</param>
        public abstract void CopyFrom(FType other);

        /// <summary>
        /// Adopt the memory to a new address.
        /// </summary>
        /// <param name="ptr">The new memory this pointer manages.</param>
        public void UpdatePtr(IntPtr ptr)
        {
            this.ptr = ptr;
            this.owned = false; // Ensure not to free someone else's memory.
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
    /// Allocate new memory in Fortran.
    /// </summary>
    /// <returns>Address of the allocated memory.</returns>
    public delegate IntPtr FTypeNew();

    /// <summary>
    /// Release memory allocated from Fortran.
    /// </summary>
    /// <param name="ptr">Address of memory to free.</param>
    public delegate void FTypeFree(IntPtr ptr);

    /// <summary>
    /// Basic type for all Fortran derived types.
    /// </summary>
    public abstract class FType
    {
        // Internal pointer to allocated memory.
        protected IntPtr ptr;

        // Flag indicating whether the memory is owned (i.e. may be freed upon disposal).
        protected Boolean owned;

        /// <summary>
        /// Get access to the plain memory.
        /// </summary>
        public IntPtr Ptr
        {
            get
            {
                return this.ptr;
            }
        }

        /// <summary>
        /// Associate a plain pointer with this type.
        /// </summary>
        /// <param name="ptr">Address of the Fortran memory.</param>
        /// <param name="owned">Indicates whether the memory is owned, i.e. will be freed by the destructor.</param>
        protected FType(IntPtr ptr, Boolean owned)
        {
            this.ptr = ptr;
            this.owned = owned;
        }

        /// <summary>
        /// Copy the data field from an identic type.
        /// </summary>
        /// <param name="other">Instance of the type.</param>
        public abstract void CopyFrom(FType other);

        /// <summary>
        /// Adopt the memory to a new address.
        /// </summary>
        /// <param name="ptr">The new memory this pointer manages.</param>
        public void UpdatePtr(IntPtr ptr)
        {
            this.ptr = ptr;
            this.owned = false; // Ensure not to free someone else's memory.
        }
    }
}
>>>>>>> Stashed changes
