#|
 This file is a part of cl-ktx
 (c) 2023 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.ktx)

(docs:define-docs
  (cl:type mipmap-level
    "Representation of a mipmap level's graphical data.

See SIZE
See DATA")

  (function make-mipmap-level
    "Create a new mipmap-level instance.

See MIPMAP-LEVEL (type)")
  
  (cl:type file
    "Representation of a KTX file.

As per spec:
KTX™ is a format for storing textures for OpenGL® and OpenGL® ES
applications. It is distinguished by the simplicity of the loader
required to instantiate a GL texture object from the file contents.

The unpack alignment is 4. I.e. uncompressed pixel data is packed
according to the rules described in section 8.4.4.1 of the OpenGL 4.4
specification [OPENGL44] for a GL_UNPACK_ALIGNMENT of 4.

Values listed in tables referred to in the OpenGL 4.4 specification
[OPENGL44] may be supplemented by extensions. The references are given
as examples and do not imply that all of those texture types can be
loaded in OpenGL ES or earlier versions of OpenGL.

Texture data in a KTX file are arranged so that the first pixel in the
data stream for each face and/or array element is closest to the
origin of the texture coordinate system. In OpenGL that origin is
conventionally described as being at the lower left, but this
convention is not shared by all image file formats and content
creation tools, so there is abundant room for confusion.

The desired texture axis orientation is often predetermined by, e.g. a
content creation tool's or existing application's use of the
image. Therefore it is strongly recommended that tools for generating
KTX files clearly describe their behaviour, and provide an option to
specify the texture axis origin and orientation relative to the
logical orientation of the source image. At minimum they should
provide a choice between top-left and bottom-left as origin for 2D
source images, with the positive S axis pointing right. Where
possible, the preferred default is to use the logical upper-left
corner of the image as the texture origin. Note that this is contrary
to the standard interpretation of GL texture coordinates. However, the
majority of texture compression tools use this convention.

As an aid to writing image manipulation tools and viewers, the logical
orientation of the data in a KTX file may be indicated in the file's
key/value metadata. Note that this metadata affects only the logical
interpretation of the data, has no effect on the mapping from pixels
in the file byte stream to texture coordinates. The recommended key to
use is:

KTXorientation
It is recommended that viewing and editing tools support at least the
following values:

  S=r,T=d
  S=r,T=u
  S=r,T=d,R=i
  S=r,T=u,R=o

where

  S indicates the direction of increasing S values
  T indicates the direction of increasing T values
  R indicates the direction of increasing R values
  r indicates increasing to the right
  l indicates increasing to the left
  d indicates increasing downwards
  u indicates increasing upwards
  o indicates increasing out from the screen (moving towards viewer)
  i indicates increasing in towards the screen (moving away from viewer)

Although other orientations can be represented, it is recommended that
tools that create KTX files use only the values listed above as other
values may not be widely supported by other tools.

See READ-FILE
See WRITE-FILE
See ENDIANNESS
See GL-TYPE
See GL-TYPE-SIZE
See GL-FORMAT
See GL-INTERNAL-FORMAT
See GL-BASE-INTERNAL-FORMAT
See WIDTH
See HEIGHT
See DEPTH
See ARRAY-ELEMENT-COUNT
See FACE-COUNT
See MIP-COUNT
See MIPMAPS
See KV-STORE
See CREATE-FILE")

  (function create-file
    "Create a new file instance.

PAYLOADS should be a vector of data vectors to use for the mipmap
levels to include in the file, stored in the correct order starting
with level 0.

If GL-INTERNAL-FORMAT, GL-BASE-INTERNAL-FORMAT, or GL-TYPE-SIZE are
not supplied, they are inferred based on the GL-TYPE and GL-FORMAT.

See FILE (type)")
  
  (function read-file
    "Reads a file from a backend supported by binary-structures.

This is, by default, one of:
  - PATHNAME
  - BINARY-STREAM
  - OCTET-VECTOR
  - FOREIGN-POINTER

See FILE (type)")
  
  (function write-file
    "Writes the file to a backend supported by binary-structures.

This is, by default, one of:
  - PATHNAME
  - BINARY-STREAM
  - OCTET-VECTOR
  - FOREIGN-POINTER

See FILE (type)")
  
  (function endianness
    "Accesses the endianness type of the file.

Must be either :LITTLE-ENDIAN or :BIG-ENDIAN

See FILE (type)")
  
  (function gl-type
    "Accesses the GL Pixel Type the data is stored as.

As per spec:
For compressed textures, glType must equal 0. For uncompressed
textures, glType specifies the type parameter passed to
glTex{,Sub}Image*D, usually one of the values from table 8.2 of the
OpenGL 4.4 specification [OPENGL44] (UNSIGNED_BYTE,
UNSIGNED_SHORT_5_6_5, etc.)

See FILE (type)")
  
  (function gl-type-size
    "Accesses the size of the GL Pixel Type in octets.

As per spec:
glTypeSize specifies the data type size that should be used when
endianness conversion is required for the texture data stored in the
file. If glType is not 0, this should be the size in bytes
corresponding to glType. For texture data which does not depend on
platform endianness, including compressed texture data, glTypeSize
must equal 1.

See FILE (type)")
  
  (function gl-format
    "Accesses the GL Pixel Format the data is stored as.

As per spec:
For compressed textures, glFormat must equal 0. For uncompressed
textures, glFormat specifies the format parameter passed to
glTex{,Sub}Image*D, usually one of the values from table 8.3 of the
OpenGL 4.4 specification [OPENGL44] (RGB, RGBA, BGRA, etc.)

See FILE (type)")
  
  (function gl-internal-format
    "Accesses the GL Internal Texture Format the data should be uploaded as.

As per spec:
For compressed textures, glInternalFormat must equal the compressed
internal format, usually one of the values from table 8.14 of the
OpenGL 4.4 specification [OPENGL44]. For uncompressed textures,
glInternalFormat specifies the internalformat parameter passed to
glTexStorage*D or glTexImage*D, usually one of the sized internal
formats from tables 8.12 & 8.13 of the OpenGL 4.4 specification
[OPENGL44]. The sized format should be chosen to match the bit depth
of the data provided. glInternalFormat is used when loading both
compressed and uncompressed textures, except when loading into a
context that does not support sized formats, such as an unextended
OpenGL ES 2.0 context where the internalformat parameter is required
to have the same value as the format parameter.

See FILE (type)")
  
  (function gl-base-internal-format
    "Accesses the basic GL Internal Texture Format the data should be uploaded as.

As per spec:
For both compressed and uncompressed textures, glBaseInternalFormat
specifies the base internal format of the texture, usually one of the
values from table 8.11 of the OpenGL 4.4 specification [OPENGL44]
(RGB, RGBA, ALPHA, etc.). For uncompressed textures, this value will
be the same as glFormat and is used as the internalformat parameter
when loading into a context that does not support sized formats, such
as an unextended OpenGL ES 2.0 context.

See FILE (type)")
  
  (function width
    "Accesses the width of the pixel data.

As per spec: 
The size of the texture image for level 0, in pixels. No rounding to
block sizes should be applied for block compressed textures.
For 1D textures pixelHeight and pixelDepth must be 0. For 2D and cube
textures pixelDepth must be 0.

See FILE (type)")
  
  (function height
    "Accesses the height of the pixel data.

As per spec: 
The size of the texture image for level 0, in pixels. No rounding to
block sizes should be applied for block compressed textures.
For 1D textures pixelHeight and pixelDepth must be 0. For 2D and cube
textures pixelDepth must be 0.

See FILE (type)")
  
  (function depth
    "Accesses the depth of the pixel data.

As per spec: 
The size of the texture image for level 0, in pixels. No rounding to
block sizes should be applied for block compressed textures.
For 1D textures pixelHeight and pixelDepth must be 0. For 2D and cube
textures pixelDepth must be 0.

See FILE (type)")
  
  (function array-element-count
    "Accesses the number of texture array elements in the data.

As per spec:
numberOfArrayElements specifies the number of array elements. If the
texture is not an array texture, numberOfArrayElements must equal 0.

See FILE (type)")
  
  (function face-count
    "Accesses the number of cubemap faces in the data.

As per spec:
numberOfFaces specifies the number of cubemap faces. For cubemaps and
cubemap arrays this should be 6. For non cubemaps this should be
1. Cube map faces are stored in the order: +X, -X, +Y, -Y, +Z, -Z.
Due to GL_OES_compressed_paletted_texture [OESCPT] not defining the
interaction between cubemaps and its GL_PALETTE* formats, if
`glInternalFormat` is one of its GL_PALETTE* format, numberOfFaces
must be 1.

See FILE (type)")
  
  (function mip-count
    "Accesses the number of mipmap levels in the data.

As per spec:
numberOfMipmapLevels must equal 1 for non-mipmapped textures. For
mipmapped textures, it equals the number of mipmaps. Mipmaps are
stored in order from largest size to smallest size. The first mipmap
level is always level 0. A KTX file does not need to contain a
complete mipmap pyramid. If numberOfMipmapLevels equals 0, it
indicates that a full mipmap pyramid should be generated from level 0
at load time (this is usually not allowed for compressed formats).

For the GL_PALETTE* formats, this equals the number of mipmaps and is
passed as the levels, parameter when uploading to OpenGL
{,ES}. However all levels are packed into a single block of data along
with the palette so numberOfMipmapLevels is considered to be 1 in the
for loop over the data. Individual mipmaps are not identifiable.

See FILE (type)
See MIPMAPS")
  
  (function mipmaps
    "Accesses the vector of mipmap storage levels.

Even the base level is stored as a mipmap-level.

See FILE (type)
See MIPMAP-LEVEL (type)")
  
  (function size
    "Accesses the size of the mipmap's data vector in octets.

As per spec:
For most textures imageSize is the number of bytes of pixel data in
the current LOD level. This includes all array layers, all z slices,
all faces, all rows (or rows of blocks) and all pixels (or blocks) in
each row for the mipmap level. It does not include any bytes in
mipPadding.

The exception is non-array cubemap textures (any texture where
numberOfFaces is 6 and numberOfArrayElements is 0). For these textures
imageSize is the number of bytes in each face of the texture for the
current LOD level, not including bytes in cubePadding or mipPadding.

See MIPMAP-LEVEL (type)")
  
  (function data
    "Accesses the mipmap level's pixel data.

As per spec:
The data is stored as follows within the opaque array:

for each array_element in max(1, arrayElementCount)
   for each face in max(1, faceCount)
       for each z_slice in max(1, depth)
           for each row or row_of_blocks in max(1, height)
               for each pixel or block_of_pixels in width
                   Byte data[format-specific-number-of-bytes]
               end
           end
       end
       Byte cubePadding[0-3]
   end
end

For non-array cubemap textures (any texture where numberOfFaces is 6
and numberOfArrayElements is 0) cubePadding contains between 0 and 3
bytes of value 0x00 to ensure that the data in each face begins at a
file offset that is a multiple of 4. In all other cases cubePadding is
empty (0 bytes long). This is empty in the non-array cubemap case as
well. The requirement of GL_UNPACK_ALIGNMENT = 4 means the size of
uncompressed textures will always be a multiple of 4 bytes. All known
compressed formats, that are usable for cubemaps, have block sizes
that are a multiple of 4 bytes. The field is still shown in case a
compressed format emerges with a block size that is not a multiple of
4 bytes.

See MIPMAP-LEVEL (type)")
  
  (function kv-store
    "Accesses the key-value store of the file.

Always returns a fresh hash-table.

Setting this will not preserve referential transparency. You may set
it as either an ALIST or a HASH-TABLE.

As per spec:
Keys that begin with the 3 ascii characters 'KTX' or 'ktx' are
reserved and must not be used except as described by this spec (this
version of the KTX spec defines a single key, see FILE).

See FILE (type)"))
