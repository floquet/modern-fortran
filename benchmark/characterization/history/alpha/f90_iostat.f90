!  f90_iostat.f90
!
!  Module defining the run-time IOSTAT values returned by the
!  NAG Fortran Compiler.
!
!  Copyright 1991-2011 The Numerical Algorithms Group Ltd., Oxford, U.K.
!
!  Malcolm Cohen, Robert Iles, July 1991, July 1993.
!
!  1.6, 95/03/15
!
!  Release 3: 1.1, 98/06/23
!
!  Release 4: 1.2, 99/01/11
!
!  Release 4.1: 1.3, 00/08/04
!
!  Release 5.1: 1.2, 06/05/25
!
!  Release 5.2: f90_iostat.f90 1 2009-12-02 02:54:28Z Malcolm Cohen
!
!  Release 5.3: $Id: f90_iostat.f90 1592 2011-09-21 04:24:45Z Malcolm Cohen $
!
Module,Intrinsic :: f90_iostat
  Use Iso_Fortran_Env,Only:ioerr_inquire_internal_unit=> & !
                           Iostat_Inquire_Internal_Unit, &
                           ioerr_eor=>Iostat_Eor, &
                           ioerr_eof=>Iostat_End
!
    Integer,Parameter :: IOERR_OK                        =   0
!
!  The NAG Fortran Compiler reserves IOSTAT values between 1 and 99
!  for host system status return values.
!  These are usually documented in the file /usr/include/sys/errno.h
!  or /usr/include/asm/errno.h.
!
!  The following IOSTAT values are used by the NAG Fortran Compiler
!  runtime system.
! 
! Buffer overflow on output    
    Integer,Parameter :: IOERR_BUFFER_OVERFLOW           = 100
! Internal file overflow
    Integer,Parameter :: IOERR_INTERNAL_FILE_OVERFLOW    = 101
! Scale factor out of range
    Integer,Parameter :: IOERR_BAD_SCALE                 = 102
! Exponent too large for w.d format
    Integer,Parameter :: IOERR_BAD_EXPONENT              = 103
! Record too long for input buffer
    Integer,Parameter :: IOERR_INPUT_BUFFER_OVERFLOW     = 104
! Zero repeat factor in list-directed input
    Integer,Parameter :: IOERR_ZERO_REPEAT               = 105
! Invalid input for integer editing
    Integer,Parameter :: IOERR_BAD_INTEGER               = 106
! Input value too large for INTEGER(KIND=1)
    Integer,Parameter :: IOERR_INTEGER1_TOO_BIG          = 107
! Input value too large for INTEGER(KIND=2)
    Integer,Parameter :: IOERR_INTEGER2_TOO_BIG          = 108
! Repeat factor in list-directed input larger than HUGE(0)
    Integer,Parameter :: IOERR_INTEGER_OVERFLOW_REPEAT   = 109
! Input value too large for default INTEGER type
    Integer,Parameter :: IOERR_INTEGER_TOO_BIG           = 110
! Invalid input for real editing
    Integer,Parameter :: IOERR_BAD_REAL                  = 111
! Invalid input for logical editing
    Integer,Parameter :: IOERR_BAD_LOGICAL               = 112
! Invalid input for complex editing
    Integer,Parameter :: IOERR_BAD_COMPLEX               = 113
! Invalid input for character editing
    Integer,Parameter :: IOERR_BAD_CHAR                  = 114
! Format specification does not begin with a left parenthesis
    Integer,Parameter :: IOERR_FORMAT_NO_LPAREN          = 115
! Format specification does not end with a right parenthesis
    Integer,Parameter :: IOERR_FORMAT_NO_ENDING_RPAREN   = 116
! No data edit descriptor in tail of format specification after reversion
    Integer,Parameter :: IOERR_NO_DATA_EDIT_IN_REVERSION = 117
! Sub-format groups nested too deeply
    Integer,Parameter :: IOERR_SUBFMT_TOO_DEEP           = 118
! Unexpected end of format specification
    Integer,Parameter :: IOERR_UNEXPECTED_FORMAT_END     = 119
! Expected integer literal constant in format specification
    Integer,Parameter :: IOERR_EXPECTED_INTEGER_VALUE    = 120
! Field/exponent width or repeat in format specification must be non-zero
    Integer,Parameter :: IOERR_FORMAT_MBNZ               = 121
! Expected decimal point in format specification
    Integer,Parameter :: IOERR_EXPECTED_PERIOD           = 122
! Expected P following signed integer constant in format specification
    Integer,Parameter :: IOERR_EXPECTED_P                = 123
! Expected N or Z following B in format specification
    Integer,Parameter :: IOERR_BAD_BNBZ                  = 124
! Invalid edit descriptor
    Integer,Parameter :: IOERR_BAD_EDIT                  = 125
! No edit descriptor following repeat factor
    Integer,Parameter :: IOERR_NO_EDIT_FOR_REPEAT        = 126
! Repeat factor given for sign edit descriptor
    Integer,Parameter :: IOERR_REPEAT_FOR_SIGN           = 127
! Repeat factor given for blank-interpretation edit descriptor
    Integer,Parameter :: IOERR_REPEAT_FOR_BLANK_INTERP   = 128
! Missing length of H edit descriptor
    Integer,Parameter :: IOERR_MISSING_HOLLERITH_LENGTH  = 129
! Repeat factor given for character string edit descriptor
    Integer,Parameter :: IOERR_REPEAT_FOR_CHAR_EDIT      = 130
! No spacing specified for X edit descriptor
    Integer,Parameter :: IOERR_NO_SPACING_FOR_X          = 131
! Repeat factor given for position edit descriptor
    Integer,Parameter :: IOERR_REPEAT_FOR_POSITION       = 132
! Character string edit descriptor used on input
    Integer,Parameter :: IOERR_CHAR_EDIT_IN_READ         = 133
! Invalid edit descriptor for real i/o-list item
    Integer,Parameter :: IOERR_BAD_EDIT_FOR_REAL         = 134
! Invalid edit descriptor for integer i/o-list item
    Integer,Parameter :: IOERR_BAD_EDIT_FOR_INTEGER      = 135
! Invalid edit descriptor for logical i/o-list item
    Integer,Parameter :: IOERR_BAD_EDIT_FOR_LOGICAL      = 136
! Character string edit descriptor does not terminate before format end
    Integer,Parameter :: IOERR_CHAR_OVERLAPS_END         = 137
! Sign in a numeric input field not followed by any digits
    Integer,Parameter :: IOERR_ONLY_SIGN_FOUND           = 138
! Invalid exponent in real input field
    Integer,Parameter :: IOERR_BAD_INPUT_EXPONENT        = 139
! Invalid character in real input field
    Integer,Parameter :: IOERR_BAD_INPUT_REAL            = 140
! Invalid character in integer input field
    Integer,Parameter :: IOERR_BAD_INPUT_INTEGER         = 141
! Invalid character in binary integer input field
    Integer,Parameter :: IOERR_BAD_BINARY                = 142
! Invalid character in octal integer input field
    Integer,Parameter :: IOERR_BAD_OCTAL                 = 143
! Invalid character in hexadecimal integer input field
    Integer,Parameter :: IOERR_BAD_HEX                   = 144
! Invalid edit descriptor for character i/o-list item
    Integer,Parameter :: IOERR_BAD_EDIT_FOR_CHARACTER    = 145
! READ after WRITE with no intervening file positioning
    Integer,Parameter :: IOERR_READ_AFTER_WRITE          = 146
! Unit number out of range
    Integer,Parameter :: IOERR_BAD_UNIT                  = 147
! Unit is not connected
    Integer,Parameter :: IOERR_NOT_CONNECTED             = 148
! File connected to unit is not capable of BACKSPACE
    Integer,Parameter :: IOERR_CANNOT_BACKSPACE          = 149
! Unit is not connected for SEQUENTIAL i/o
    Integer,Parameter :: IOERR_NOT_SEQUENTIAL            = 150
! Unit is not connected for READ action
    Integer,Parameter :: IOERR_NOT_READ                  = 151
! Unit is not connected for FORMATTED i/o
    Integer,Parameter :: IOERR_NOT_FORMATTED             = 152
! Unit is not connected for WRITE action
    Integer,Parameter :: IOERR_NOT_WRITE                 = 153
! Unit is not connected for UNFORMATTED i/o
    Integer,Parameter :: IOERR_NOT_UNFORMATTED           = 154
! Unit is not connected on OPEN with STATUS='OLD' and no FILE= specifier
    Integer,Parameter :: IOERR_OLD_UNCONNECTED_NEED_FILE = 155
! FILE= specifier on OPEN with STATUS='SCRATCH'
    Integer,Parameter :: IOERR_SCRATCH_NAMED             = 156
! OPEN on connected unit with STATUS= specifier must have STATUS='OLD'
    Integer,Parameter :: IOERR_DIFFERENT_STATUS          = 157
! OPEN on connected unit has different ACCESS= specifier
    Integer,Parameter :: IOERR_DIFFERENT_ACCESS          = 158
! OPEN on connected unit has different FORM= specifier
    Integer,Parameter :: IOERR_DIFFERENT_FORM            = 159
! OPEN on connected unit has different RECL= specifier
    Integer,Parameter :: IOERR_DIFFERENT_RECL            = 160
! OPEN on connected unit has different ACTION= specifier
    Integer,Parameter :: IOERR_DIFFERENT_ACTION          = 161
! OPEN on connected unit has different POSITION= specifier
    Integer,Parameter :: IOERR_DIFFERENT_POSITION        = 162
! Invalid value for STATUS= specifier
    Integer,Parameter :: IOERR_BAD_STATUS                = 163
! Invalid value for ACCESS= specifier
    Integer,Parameter :: IOERR_BAD_ACCESS                = 164
! Invalid value for FORM= specifier
    Integer,Parameter :: IOERR_BAD_FORM                  = 165
! Invalid value for BLANKS= specifier
    Integer,Parameter :: IOERR_BAD_BLANKS                = 166
! Invalid value for POSITION= specifier
    Integer,Parameter :: IOERR_BAD_POSITION              = 167
! INvalid value for ACTION= specifier
    Integer,Parameter :: IOERR_BAD_ACTION                = 168
! Invalid value for DELIM= specifier
    Integer,Parameter :: IOERR_BAD_DELIM                 = 169
! Invalid value for PAD= specifier
    Integer,Parameter :: IOERR_BAD_PAD                   = 170
! The RECL= specifier must be given for DIRECT access OPEN
    Integer,Parameter :: IOERR_NO_RECL                   = 171
! STATUS='KEEP' is invalid for a SCRATCH file
    Integer,Parameter :: IOERR_CANNOT_KEEP               = 172
! ENDFILE applied twice to unit with no intervening file positioning
    Integer,Parameter :: IOERR_ENDFILE_TWICE             = 173
! File name too long
    Integer,Parameter :: IOERR_NAME_TOO_LONG             = 174
! Cannot find OLD file
    Integer,Parameter :: IOERR_NO_OLD_FILE               = 175
! NeW file already exists
    Integer,Parameter :: IOERR_NEW_FILE_EXISTS           = 176
! File connected to unit is not capable of REWIND
    Integer,Parameter :: IOERR_CANNOT_REWIND             = 177
! BACKSPACE failed to find the beginning of the previous record
    Integer,Parameter :: IOERR_BACKSPACE_FAILED          = 178
! Unit is not connected for DIRECT i/o
    Integer,Parameter :: IOERR_NOT_DIRECT                = 179
! Record number out of range
    Integer,Parameter :: IOERR_BAD_REC                   = 180
! Illegal character in LOGICAL input field
    Integer,Parameter :: IOERR_BAD_INPUT_LOGICAL         = 181
! No value found in LOGICAL input field
    Integer,Parameter :: IOERR_NO_INPUT_LOGICAL          = 182
! Unknown OPEN failure on unit
    Integer,Parameter :: IOERR_CANNOT_OPEN               = 183
! Expected '&' but found '%c' in NAMELIST input
    Integer,Parameter :: IOERR_NO_AMPERSAND              = 184
! NAMELIST group name in input is too long
    Integer,Parameter :: IOERR_GROUP_NAME_TOO_LONG       = 185
! Expected NAMELIST group /%s/ but found /%s/
    Integer,Parameter :: IOERR_WRONG_NAMELIST            = 186
! Invalid character '%c' in NAMELIST input
    Integer,Parameter :: IOERR_NAMELIST_BAD_CHAR         = 187
! NAMELIST group name in input of NAMELIST/%s/ is too long
    Integer,Parameter :: IOERR_OBJECT_NAME_TOO_LONG      = 188
! Expected '=' but found '%c' in NAMELIST input
    Integer,Parameter :: IOERR_EXPECTED_EQUALS           = 189
! Unknown group object \"%s\" in input for NAMELIST/%s/
    Integer,Parameter :: IOERR_UNKNOWN_OBJECT_NAME       = 190
! Unexpected subscript for object %s of NAMELIST/%s/
    Integer,Parameter :: IOERR_UNEXPECTED_SUBSCRIPT      = 191
! Unexpected component specifier for object %s of NAMELIST/%s/
    Integer,Parameter :: IOERR_UNEXPECTED_COMPONENT      = 192
! Component name too long in input for object %s of NAMELIST/%s/
    Integer,Parameter :: IOERR_COMPONENT_NAME_TOO_LONG   = 193
! Unknown component \"%s\" in input for object %s of NAMELIST/%s/
    Integer,Parameter :: IOERR_UNKNOWN_COMPONENT         = 194
! Array component of array parent in input for object %s of NAMELIST/%s/
    Integer,Parameter :: IOERR_ARRAY_OF_ARRAY            = 195
! Invalid integer literal in input for object %s of NAMELIST/%s/
    Integer,Parameter :: IOERR_BAD_INTEGER_LITERAL       = 196
! Expected ':' but found '%c' in input for object %s of NAMELIST/%s/
    Integer,Parameter :: IOERR_EXPECTED_COLON            = 197
! Substring has zero length in input for object %s of NAMELIST/%s/
    Integer,Parameter :: IOERR_ZERO_LENGTH_INPUT         = 198
! Substring (%d:%d) out of bounds in input for object %s of NAMELIST/%s/
    Integer,Parameter :: IOERR_SUBSTRING_OUT_OF_BOUNDS   = 199
! Expected ',' but found '%c' in input for object %s of NAMELIST/%s/
    Integer,Parameter :: IOERR_EXPECTED_COMMA            = 200
! Expected ')' but found '%c' in input for object %s of NAMELIST/%s/
    Integer,Parameter :: IOERR_EXPECTED_RPAREN           = 201
! Subscript (%d) out of range in input for object %s of NAMELIST/%s/
    Integer,Parameter :: IOERR_SUBSCRIPT_OUT_OF_RANGE    = 202
! Array section has zero size in input for object %s of NAMELIST/%s/
    Integer,Parameter :: IOERR_ZERO_SIZE_INPUT           = 203
! Section stride has value zero in input for object %s of NAMELIST/%s/
    Integer,Parameter :: IOERR_ZERO_STRIDE               = 204
! Missing namelist group name in input of NAMELIST/%s/
    Integer,Parameter :: IOERR_NO_NAMELIST_GROUP_NAME    = 205
! Input list bigger than record length in unformatted READ on unit %d
    Integer,Parameter :: IOERR_INPUT_LIST_TOO_BIG        = 206
! Record too short for format requirement and PAD='NO' on unit %d
    Integer,Parameter :: IOERR_RECORD_TOO_SHORT          = 207
! Unformatted data file open on unit %d is corrupt
    Integer,Parameter :: IOERR_CORRUPT_UNFORMATTED_FILE  = 208
! File truncation on unit %d failed
    Integer,Parameter :: IOERR_TRUNCATE_FAILED           = 209
! READ/WRITE attempted after ENDFILE on unit %d
    Integer,Parameter :: IOERR_RW_AFTER_ENDFILE          = 210
! Input value too large for 64-bit integer
    Integer,Parameter :: IOERR_INTEGER64_TOO_BIG         = 211
! No FILE= specifier with STATUS='REPLACE' or STATUS='NEW'
    Integer,Parameter :: IOERR_REPLACE_OR_NEW_NEED_FILE  = 212
! Invalid value for RECL= specifier (must be positive)
    Integer,Parameter :: IOERR_RECL_LE_ZERO              = 213
! READ beyond end of direct access file on unit %d
    Integer,Parameter :: IOERR_END_OF_DIRECT_ACCESS      = 214
! Floating overflow on real number input
    Integer,Parameter :: IOERR_REAL_INPUT_OVERFLOW       = 215
! Direct access is incompatible with the POSITION= specifier
    Integer,Parameter :: IOERR_DIRECT_POSITION_INCOMPAT  = 216
! Scale factors cannot be followed by repeat count in FMT= specifier
    Integer,Parameter :: IOERR_SCALE_FOLLOWED_BY_REPEAT  = 217
! RECL= specifier with ACCESS='STREAM'
    Integer,Parameter :: IOERR_RECL_WITH_STREAM_ACCESS   = 218
! Cannot BACKSPACE unformatted ACCESS='STREAM' unit %d
    Integer,Parameter :: IOERR_BACKSPACE_OF_UNF_STREAM   = 219
! ADVANCE= specifier must be 'YES' or 'NO'
    Integer,Parameter :: IOERR_BAD_ADVANCE               = 220
! SIZE= is not valid without ADVANCE='NO'
    Integer,Parameter :: IOERR_SIZE_WITHOUT_NONADVANCING = 221
! DECIMAL= specifier must be 'POINT' or 'COMMA'
    Integer,Parameter :: IOERR_BAD_DECIMAL               = 222
! Invalid value for ROUND= specifier
    Integer,Parameter :: IOERR_BAD_ROUND                 = 223
! SIGN= specifier must be  'PROCESSOR_DEFINED', 'PLUS' or 'SUPPRESS'
    Integer,Parameter :: IOERR_BAD_SIGN                  = 224
! ASYNCHRONOUS= specifier must be 'NO' or 'YES'
    Integer,Parameter :: IOERR_BAD_ASYNCHRONOUS          = 225
! Invalid value for ENCODING= specifier
    Integer,Parameter :: IOERR_BAD_ENCODING              = 226
! PENDING= specifier but unit %d is not open for ASYNCHRONOUS i/o
    Integer,Parameter :: IOERR_PENDING_BUT_NOT_ASYNC     = 227
! POS= specifier but unit %d is not open for STREAM i/o
    Integer,Parameter :: IOERR_POS_BUT_NOT_STREAM        = 228
! Invalid value for POS= specifier
    Integer,Parameter :: IOERR_BAD_POS                   = 229
! OPEN on connected unit %d has different ASYNCHRONOUS= specifier
    Integer,Parameter :: IOERR_DIFFERENT_ASYNCHRONOUS    = 230
! DELIM= specifier in OPEN for an UNFORMATTED file
    Integer,Parameter :: IOERR_DELIM_on_unformatted_OPEN = 231
! PAD= specifier in OPEN for an UNFORMATTED file
    Integer,Parameter :: IOERR_PAD_on_unformatted_OPEN   = 232
! WAIT with ID= but unit is open with ASYNCHRONOUS='NO'
    Integer,Parameter :: IOERR_WAIT_WITH_ID_BUT_FILE_IS_NOT_ASYNC = 233
! WAIT with ID= but unit is not connected to a file
    Integer,Parameter :: IOERR_WAIT_WITH_ID_BUT_NO_CONNECTION = 234
! Invalid value for ID= specifier
    Integer,Parameter :: IOERR_INVALID_VALUE_FOR_ID_SPECIFIER = 235
    Integer,Parameter :: IOERR_BAD_CONVERT_SPECIFIER     = 236
! Invalid value for CONVERT= specifier.
    Integer,Parameter :: IOERR_CONVERT_on_formatted_OPEN = 237
! CONVERT= specifier in OPEN for a FORMATTED file.
    Integer,Parameter :: IOERR_CONVERT_SPECIFIER_MODE_NOT_SUPPORTED = 238
! Specified CONVERT= mode is not supported.
    Integer,Parameter :: IOERR_BAD_ENCODIONG            = 239
! Contents of formatted file are not valid for the current ENCODING.
    Integer,Parameter :: IOERR_NO_NEWUNIT_AVAILABLE     = 240
! No unit available for NEWUNIT= specifier.
    Integer,Parameter :: IOERR_NEWUNIT_BUT_NO_FILE_AND_NOT_STATUS_SCRATCH = 241
! NEWUNIT= specifier but no FILE= and STATUS= value is not 'SCRATCH'.
!    Integer,Parameter :: IOERR_INQUIRE_INTERNAL_UNIT    = 242
! INQUIRE on a unit number that identifies an internal file.
End Module f90_iostat
