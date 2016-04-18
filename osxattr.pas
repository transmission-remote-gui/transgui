unit osxattr;

{$mode delphi}

interface

uses
  BaseUnix,
  ctypes;

const
  FSOPT_NOFOLLOW 		      = $00000001;
  FSOPT_NOINMEMUPDATE 	  = $00000002;
  FSOPT_REPORT_FULLSIZE	  = $00000004;
  { The following option only valid when requesting ATTR_CMN_RETURNED_ATTRS }
  FSOPT_PACK_INVAL_ATTRS  = $00000008;

{ we currently aren't anywhere near this amount for a valid
 * fssearchblock.sizeofsearchparams1 or fssearchblock.sizeofsearchparams2
 * but we put a sanity check in to avoid abuse of the value passed in from
 * user land.
 }
  SEARCHFS_MAX_SEARCHPARMS = 4096;

type
  u_short   = cushort;
  u_int16_t = cuint16;
  int32_t   = cint16;
  u_int32_t = cuint32;
  u_long    = culong;
  u_char    = cuchar;

  text_encoding_t = u_int32_t;
  fsobj_type_t = u_int32_t;
  fsobj_tag_t = u_int32_t;
  fsfile_type_t = u_int32_t;
  fsvolid_t = u_int32_t;

  fsobj_id_t = packed record
  	fid_objno      : u_int32_t;
    fid_generation : u_int32_t;
  end;
  pfsobj_id_t = ^fsobj_id_t;

  attrgroup_t = u_int32_t;

  attrlist = packed record
  	bitmapcount : u_short;			{ number of attr. bit sets in list (should be 5) }
	  reserved    : u_int16_t;				{ (to maintain 4-byte alignment) }
  	commonattr  : attrgroup_t;			{ common attribute group }
  	volattr     : attrgroup_t;			{ Volume attribute group }
  	dirattr     : attrgroup_t;			{ directory attribute group }
  	fileattr    : attrgroup_t;			{ file attribute group }
  	forkattr    : attrgroup_t;			{ fork attribute group }
  end;
  pattrlist = ^attrlist;

const
  ATTR_BIT_MAP_COUNT = 5;

type
  attribute_set_t = packed record
	  commonattr  : attrgroup_t;			{ common attribute group }
	  volattr     : attrgroup_t;			{ Volume attribute group }
	  dirattr     : attrgroup_t;			{ directory attribute group }
	  fileattr    : attrgroup_t;			{ file attribute group }
	  forkattr    : attrgroup_t;			{ fork attribute group }
  end;
  pattribute_set_t = ^attribute_set_t;

  attrreference_t = packed record
  	attr_dataoffset : int32_t;
  	attr_length     : u_int32_t;
  end;
  pattrreference_t = ^attrreference_t;

{ XXX PPD This is derived from HFSVolumePriv.h and should perhaps be referenced from there? }

  diskextent = packed record
	  startblock  : u_int32_t;				{ first block allocated }
	  blockcount  : u_int32_t;				{ number of blocks allocated }
  end;
  pdiskextent = ^diskextent;

  extentrecord = array [0..7] of diskextent;
  vol_capabilities_set_t = array [0..3] of u_int32_t;

const
  VOL_CAPABILITIES_FORMAT     = 0;
  VOL_CAPABILITIES_INTERFACES = 1;
  VOL_CAPABILITIES_RESERVED1  = 2;
  VOL_CAPABILITIES_RESERVED2  = 3;

type
  vol_capabilities_attr_t = packed record
  	capabilities  : vol_capabilities_set_t;
	  valid         : vol_capabilities_set_t;
  end;
  pvol_capabilities_attr_t = ^vol_capabilities_attr_t;

{
 * XXX this value needs to be raised - 3893388
 }
const
  ATTR_MAX_BUFFER		= 8192;

{
 * VOL_CAP_FMT_PERSISTENTOBJECTIDS: When set, the volume has object IDs
 * that are persistent (retain their values even when the volume is
 * unmounted and remounted), and a file or directory can be looked up
 * by ID.  Volumes that support VolFS and can support Carbon File ID
 * references should set this bit.
 *
 * VOL_CAP_FMT_SYMBOLICLINKS: When set, the volume supports symbolic
 * links.  The symlink(), readlink(), and lstat() calls all use this
 * symbolic link.
 *
 * VOL_CAP_FMT_HARDLINKS: When set, the volume supports hard links.
 * The link() call creates hard links.
 *
 * VOL_CAP_FMT_JOURNAL: When set, the volume is capable of supporting
 * a journal used to speed recovery in case of unplanned shutdown
 * (such as a power outage or crash).  This bit does not necessarily
 * mean the volume is actively using a journal for recovery.
 *
 * VOL_CAP_FMT_JOURNAL_ACTIVE: When set, the volume is currently using
 * a journal for use in speeding recovery after an unplanned shutdown.
 * This bit can be set only if VOL_CAP_FMT_JOURNAL is also set.
 *
 * VOL_CAP_FMT_NO_ROOT_TIMES: When set, the volume format does not
 * store reliable times for the root directory, so you should not
 * depend on them to detect changes, etc.
 *
 * VOL_CAP_FMT_SPARSE_FILES: When set, the volume supports sparse files.
 * That is, files which can have "holes" that have never been written
 * to, and are not allocated on disk.  Sparse files may have an
 * allocated size that is less than the file's logical length.
 *
 * VOL_CAP_FMT_ZERO_RUNS: For security reasons, parts of a file (runs)
 * that have never been written to must appear to contain zeroes.  When
 * this bit is set, the volume keeps track of allocated but unwritten
 * runs of a file so that it can substitute zeroes without actually
 * writing zeroes to the media.  This provides performance similar to
 * sparse files, but not the space savings.
 *
 * VOL_CAP_FMT_CASE_SENSITIVE: When set, file and directory names are
 * case sensitive (upper and lower case are different).  When clear,
 * an upper case character is equivalent to a lower case character,
 * and you can't have two names that differ solely in the case of
 * the characters.
 *
 * VOL_CAP_FMT_CASE_PRESERVING: When set, file and directory names
 * preserve the difference between upper and lower case.  If clear,
 * the volume may change the case of some characters (typically
 * making them all upper or all lower case).  A volume that sets
 * VOL_CAP_FMT_CASE_SENSITIVE should also set VOL_CAP_FMT_CASE_PRESERVING.
 *
 * VOL_CAP_FMT_FAST_STATFS: This bit is used as a hint to upper layers
 * (especially Carbon) that statfs() is fast enough that its results
 * need not be cached by those upper layers.  A volume that caches
 * the statfs information in its in-memory structures should set this bit.
 * A volume that must always read from disk or always perform a network
 * transaction should not set this bit.
 *
 * VOL_CAP_FMT_2TB_FILESIZE: If this bit is set the volume format supports
 * file sizes larger than 4GB, and potentially up to 2TB; it does not
 * indicate whether the filesystem supports files larger than that.
 *
 * VOL_CAP_FMT_OPENDENYMODES: When set, the volume supports open deny
 * modes (e.g. "open for read write, deny write"; effectively, mandatory
 * file locking based on open modes).
 *
 * VOL_CAP_FMT_HIDDEN_FILES: When set, the volume supports the UF_HIDDEN
 * file flag, and the UF_HIDDEN flag is mapped to that volume's native
 * "hidden" or "invisible" bit (which may be the invisible bit from the
 * Finder Info extended attribute).
 *
 * VOL_CAP_FMT_PATH_FROM_ID:  When set, the volume supports the ability
 * to derive a pathname to the root of the file system given only the
 * id of an object.  This also implies that object ids on this file
 * system are persistent and not recycled.  This is a very specialized
 * capability and it is assumed that most file systems will not support
 * it.  Its use is for legacy non-posix APIs like ResolveFileIDRef.
 *
 * VOL_CAP_FMT_NO_VOLUME_SIZES: When set, the volume does not support
 * returning values for total data blocks, available blocks, or free blocks
 * (as in f_blocks, f_bavail, or f_bfree in "struct statfs").  Historically,
 * those values were set to $FFFFFFFF for volumes that did not support them.
 *
 * VOL_CAP_FMT_DECMPFS_COMPRESSION: When set, the volume supports transparent
 * decompression of compressed files using decmpfs.
 }
  VOL_CAP_FMT_PERSISTENTOBJECTIDS	=	$00000001;
  VOL_CAP_FMT_SYMBOLICLINKS 		  =	$00000002;
  VOL_CAP_FMT_HARDLINKS      	    =	$00000004;
  VOL_CAP_FMT_JOURNAL       	    =	$00000008;
  VOL_CAP_FMT_JOURNAL_ACTIVE 	    =	$00000010;
  VOL_CAP_FMT_NO_ROOT_TIMES 	    =	$00000020;
  VOL_CAP_FMT_SPARSE_FILES 		    =	$00000040;
  VOL_CAP_FMT_ZERO_RUNS     	    =	$00000080;
  VOL_CAP_FMT_CASE_SENSITIVE 	    =	$00000100;
  VOL_CAP_FMT_CASE_PRESERVING   	=	$00000200;
  VOL_CAP_FMT_FAST_STATFS 		    =	$00000400;
  VOL_CAP_FMT_2TB_FILESIZE		    =	$00000800;
  VOL_CAP_FMT_OPENDENYMODES		    =	$00001000;
  VOL_CAP_FMT_HIDDEN_FILES		    =	$00002000;
  VOL_CAP_FMT_PATH_FROM_ID		    =	$00004000;
  VOL_CAP_FMT_NO_VOLUME_SIZES	    =	$00008000;
  VOL_CAP_FMT_DECMPFS_COMPRESSION =	$00010000;


{
 * VOL_CAP_INT_SEARCHFS: When set, the volume implements the
 * searchfs() system call (the vnop_searchfs vnode operation).
 *
 * VOL_CAP_INT_ATTRLIST: When set, the volume implements the
 * getattrlist() and setattrlist() system calls (vnop_getattrlist
 * and vnop_setattrlist vnode operations) for the volume, files,
 * and directories.  The volume may or may not implement the
 * readdirattr() system call.  XXX Is there any minimum set
 * of attributes that should be supported?  To determine the
 * set of supported attributes, get the ATTR_VOL_ATTRIBUTES
 * attribute of the volume.
 *
 * VOL_CAP_INT_NFSEXPORT: When set, the volume implements exporting
 * of NFS volumes.
 *
 * VOL_CAP_INT_READDIRATTR: When set, the volume implements the
 * readdirattr() system call (vnop_readdirattr vnode operation).
 *
 * VOL_CAP_INT_EXCHANGEDATA: When set, the volume implements the
 * exchangedata() system call (VNOP_EXCHANGE vnode operation).
 *
 * VOL_CAP_INT_COPYFILE: When set, the volume implements the
 * VOP_COPYFILE vnode operation.  (XXX There should be a copyfile()
 * system call in <unistd.h>.)
 *
 * VOL_CAP_INT_ALLOCATE: When set, the volume implements the
 * VNOP_ALLOCATE vnode operation, which means it implements the
 * F_PREALLOCATE selector of fcntl(2).
 *
 * VOL_CAP_INT_VOL_RENAME: When set, the volume implements the
 * ATTR_VOL_NAME attribute for both getattrlist() and setattrlist().
 * The volume can be renamed by setting ATTR_VOL_NAME with setattrlist().
 *
 * VOL_CAP_INT_ADVLOCK: When set, the volume implements POSIX style
 * byte range locks via vnop_advlock (accessible from fcntl(2)).
 *
 * VOL_CAP_INT_FLOCK: When set, the volume implements whole-file flock(2)
 * style locks via vnop_advlock.  This includes the O_EXLOCK and O_SHLOCK
 * flags of the open(2) call.
 *
 * VOL_CAP_INT_EXTENDED_SECURITY: When set, the volume implements
 * extended security (ACLs).
 *
 * VOL_CAP_INT_USERACCESS:  When set, the volume supports the
 * ATTR_CMN_USERACCESS attribute (used to get the user's access
 * mode to the file).
 *
 * VOL_CAP_INT_MANLOCK: When set, the volume supports AFP-style
 * mandatory byte range locks via an ioctl().
 *
 * VOL_CAP_INT_EXTENDED_ATTR: When set, the volume implements
 * native extended attribues.
 *
 * VOL_CAP_INT_NAMEDSTREAMS: When set, the volume supports
 * native named streams.
 }
  VOL_CAP_INT_SEARCHFS		 = $00000001;
  VOL_CAP_INT_ATTRLIST		 = $00000002;
  VOL_CAP_INT_NFSEXPORT		 = $00000004;
  VOL_CAP_INT_READDIRATTR	 = $00000008;
  VOL_CAP_INT_EXCHANGEDATA = $00000010;
  VOL_CAP_INT_COPYFILE	 	 = $00000020;
  VOL_CAP_INT_ALLOCATE	 	 = $00000040;
  VOL_CAP_INT_VOL_RENAME 	 = $00000080;
  VOL_CAP_INT_ADVLOCK		 	 = $00000100;
  VOL_CAP_INT_FLOCK			   = $00000200;
  VOL_CAP_INT_EXTENDED_SECURITY	= $00000400;
  VOL_CAP_INT_USERACCESS			  = $00000800;
  VOL_CAP_INT_MANLOCK			      = $00001000;
  VOL_CAP_INT_NAMEDSTREAMS		  = $00002000;
  VOL_CAP_INT_EXTENDED_ATTR		  = $00004000;

type
  vol_attributes_attr_t = packed record
	  validattr  : attribute_set_t;
  	nativeattr : attribute_set_t;
  end;
  pvol_attributes_attr_t = ^vol_attributes_attr_t;

const
  ATTR_CMN_NAME			    	= $00000001;
  ATTR_CMN_DEVID				  = $00000002;
  ATTR_CMN_FSID				    = $00000004;
  ATTR_CMN_OBJTYPE			  = $00000008;
  ATTR_CMN_OBJTAG				  = $00000010;
  ATTR_CMN_OBJID				  = $00000020;
  ATTR_CMN_OBJPERMANENTID	= $00000040;
  ATTR_CMN_PAROBJID				= $00000080;
  ATTR_CMN_SCRIPT					= $00000100;
  ATTR_CMN_CRTIME					= $00000200;
  ATTR_CMN_MODTIME				= $00000400;
  ATTR_CMN_CHGTIME				= $00000800;
  ATTR_CMN_ACCTIME				= $00001000;
  ATTR_CMN_BKUPTIME				= $00002000;
  ATTR_CMN_FNDRINFO				= $00004000;
  ATTR_CMN_OWNERID				= $00008000;
  ATTR_CMN_GRPID					= $00010000;
  ATTR_CMN_ACCESSMASK			= $00020000;
  ATTR_CMN_FLAGS				  = $00040000;
{    ATTR_CMN_NAMEDATTRCOUNT =	$00080000;	 not implemented }
{    ATTR_CMN_NAMEDATTRLIST	= $00100000;	 not implemented }
  ATTR_CMN_USERACCESS			   = $00200000;
  ATTR_CMN_EXTENDED_SECURITY = $00400000;
  ATTR_CMN_UUID			  	= $00800000;
  ATTR_CMN_GRPUUID			= $01000000;
  ATTR_CMN_FILEID				= $02000000;
  ATTR_CMN_PARENTID			= $04000000;
  ATTR_CMN_FULLPATH			= $08000000;
{
 * ATTR_CMN_RETURNED_ATTRS is only valid with getattrlist(2).
 * It is always the first attribute in the return buffer.
 }
  ATTR_CMN_RETURNED_ATTRS		=	$80000000;

  ATTR_CMN_VALIDMASK			  = $8FE7FFFF;
  ATTR_CMN_SETMASK			    = $01C7FF00;
  ATTR_CMN_VOLSETMASK			  = $00006700;

  ATTR_VOL_FSTYPE				  = $00000001;
  ATTR_VOL_SIGNATURE			= $00000002;
  ATTR_VOL_SIZE				    = $00000004;
  ATTR_VOL_SPACEFREE			= $00000008;
  ATTR_VOL_SPACEAVAIL			= $00000010;
  ATTR_VOL_MINALLOCATION		=	$00000020;
  ATTR_VOL_ALLOCATIONCLUMP	=	$00000040;
  ATTR_VOL_IOBLOCKSIZE			= $00000080;
  ATTR_VOL_OBJCOUNT			  =	$00000100;
  ATTR_VOL_FILECOUNT			=	$00000200;
  ATTR_VOL_DIRCOUNT			  = $00000400;
  ATTR_VOL_MAXOBJCOUNT		=	$00000800;
  ATTR_VOL_MOUNTPOINT			=	$00001000;
  ATTR_VOL_NAME				    = $00002000;
  ATTR_VOL_MOUNTFLAGS			=	$00004000;
  ATTR_VOL_MOUNTEDDEVICE	=	$00008000;
  ATTR_VOL_ENCODINGSUSED	= $00010000;
  ATTR_VOL_CAPABILITIES		=	$00020000;
  ATTR_VOL_UUID				    =	$00040000;
  ATTR_VOL_ATTRIBUTES			= $40000000;
  ATTR_VOL_INFO				    = $80000000;

  ATTR_VOL_VALIDMASK			= $C007FFFF;
  ATTR_VOL_SETMASK			  = $80002000;


{ File/directory attributes: }
  ATTR_DIR_LINKCOUNT			= $00000001;
  ATTR_DIR_ENTRYCOUNT			= $00000002;
  ATTR_DIR_MOUNTSTATUS		= $00000004;
  DIR_MNTSTATUS_MNTPOINT	= $00000001;

  ATTR_DIR_VALIDMASK			= $00000007;
  ATTR_DIR_SETMASK			  = $00000000;

  ATTR_FILE_LINKCOUNT			= $00000001;
  ATTR_FILE_TOTALSIZE			= $00000002;
  ATTR_FILE_ALLOCSIZE			= $00000004;
  ATTR_FILE_IOBLOCKSIZE		= $00000008;
  ATTR_FILE_DEVTYPE			  = $00000020;
  ATTR_FILE_FORKCOUNT			= $00000080;
  ATTR_FILE_FORKLIST			= $00000100;
  ATTR_FILE_DATALENGTH		= $00000200;
  ATTR_FILE_DATAALLOCSIZE	= $00000400;
  ATTR_FILE_RSRCLENGTH		= $00001000;
  ATTR_FILE_RSRCALLOCSIZE	= $00002000;

  ATTR_FILE_VALIDMASK			= $000037FF;
  ATTR_FILE_SETMASK			  = $00000020;

  ATTR_FORK_TOTALSIZE			= $00000001;
  ATTR_FORK_ALLOCSIZE			= $00000002;

  ATTR_FORK_VALIDMASK			= $00000003;
  ATTR_FORK_SETMASK			  = $00000000;

{ Obsolete, implemented, not supported }
  ATTR_CMN_NAMEDATTRCOUNT	= $00080000;	{ not implemented }
  ATTR_CMN_NAMEDATTRLIST	= $00100000;	{ not implemented }
  ATTR_FILE_CLUMPSIZE			= $00000010;	{ obsolete }
  ATTR_FILE_FILETYPE			= $00000040;	{ always zero }
  ATTR_FILE_DATAEXTENTS		= $00000800;	{ obsolete, HFS-specific }
  ATTR_FILE_RSRCEXTENTS		= $00004000;	{ obsolete, HFS-specific }

{
 * Searchfs
 }
  SRCHFS_START 				      = $00000001;
  SRCHFS_MATCHPARTIALNAMES 	= $00000002;
  SRCHFS_MATCHDIRS 			    = $00000004;
  SRCHFS_MATCHFILES 			  = $00000008;
  SRCHFS_SKIPLINKS 			    = $00000010;
  SRCHFS_SKIPINVISIBLE			= $00000020;
  SRCHFS_SKIPPACKAGES			  = $00000040;
  SRCHFS_SKIPINAPPROPRIATE	= $00000080;

  SRCHFS_NEGATEPARAMS 			= $80000000;
  SRCHFS_VALIDOPTIONSMASK		= $800000FF;

type
  fssearchblock = packed record
  	returnattrs         : pattrlist;
  	returnbuffer        : Pointer;
  	returnbuffersize    : size_t;
  	maxmatches          : u_long;
  	timelimit           : timeval;
  	searchparams1       : Pointer;
  	sizeofsearchparams1 : size_t;
  	searchparams2       : Pointer;
  	sizeofsearchparams2 : size_t;
  	searchattrs         : attrlist;
  end;
  pfssearchblock=^fssearchblock;

  searchstate = packed record
	  reserved : array [0..555] of u_char;		//	sizeof( SearchState )
  end;
  psearchstate = ^searchstate;

function getattrlist(path: Pchar; alist: pattrlist; attrBuf: Pointer; attrBufSize: size_t; options: LongWord): Integer; cdecl; external;

implementation

end.

