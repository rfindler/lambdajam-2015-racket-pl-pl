# 1 "sqr.c"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "sqr.c"

# 1 "/usr/include/stdio.h" 1 3 4

# 23 "/usr/include/stdio.h" 3 4

# 60 "/usr/include/stdio.h" 3 4





# 1 "/usr/include/_types.h" 1 3 4

# 23 "/usr/include/_types.h" 3 4





# 1 "/usr/include/sys/_types.h" 1 3 4

# 28 "/usr/include/sys/_types.h" 3 4





# 1 "/usr/include/sys/cdefs.h" 1 3 4

# 28 "/usr/include/sys/cdefs.h" 3 4


# 66 "/usr/include/sys/cdefs.h" 3 4













# 105 "/usr/include/sys/cdefs.h" 3 4









# 128 "/usr/include/sys/cdefs.h" 3 4


# 155 "/usr/include/sys/cdefs.h" 3 4


# 166 "/usr/include/sys/cdefs.h" 3 4


# 180 "/usr/include/sys/cdefs.h" 3 4


# 198 "/usr/include/sys/cdefs.h" 3 4




















# 253 "/usr/include/sys/cdefs.h" 3 4








# 285 "/usr/include/sys/cdefs.h" 3 4

# 307 "/usr/include/sys/cdefs.h" 3 4

# 329 "/usr/include/sys/cdefs.h" 3 4

# 339 "/usr/include/sys/cdefs.h" 3 4






# 353 "/usr/include/sys/cdefs.h" 3 4

# 363 "/usr/include/sys/cdefs.h" 3 4

# 373 "/usr/include/sys/cdefs.h" 3 4


































# 416 "/usr/include/sys/cdefs.h" 3 4



# 436 "/usr/include/sys/cdefs.h" 3 4














# 459 "/usr/include/sys/cdefs.h" 3 4



















# 509 "/usr/include/sys/cdefs.h" 3 4































# 548 "/usr/include/sys/cdefs.h" 3 4


























# 582 "/usr/include/sys/cdefs.h" 3 4

# 33 "/usr/include/sys/_types.h" 2 3 4

# 1 "/usr/include/machine/_types.h" 1 3 4

# 30 "/usr/include/machine/_types.h" 3 4





# 1 "/usr/include/i386/_types.h" 1 3 4

# 30 "/usr/include/i386/_types.h" 3 4







typedef __signed char		__int8_t;



typedef unsigned char		__uint8_t;
typedef	short			__int16_t;
typedef	unsigned short		__uint16_t;
typedef int			__int32_t;
typedef unsigned int		__uint32_t;
typedef long long		__int64_t;
typedef unsigned long long	__uint64_t;

typedef long			__darwin_intptr_t;
typedef unsigned int		__darwin_natural_t;


# 69 "/usr/include/i386/_types.h" 3 4

typedef int			__darwin_ct_rune_t;	





typedef union {
	char		__mbstate8[128];
	long long	_mbstateL;			
} __mbstate_t;

typedef __mbstate_t		__darwin_mbstate_t;	


typedef long int	__darwin_ptrdiff_t;	





typedef long unsigned int		__darwin_size_t;	





typedef __builtin_va_list	__darwin_va_list;	





typedef int		__darwin_wchar_t;	




typedef __darwin_wchar_t	__darwin_rune_t;	


typedef int		__darwin_wint_t;	




typedef unsigned long		__darwin_clock_t;	
typedef __uint32_t		__darwin_socklen_t;	
typedef long			__darwin_ssize_t;	
typedef long			__darwin_time_t;	

# 35 "/usr/include/machine/_types.h" 2 3 4






# 34 "/usr/include/sys/_types.h" 2 3 4


# 57 "/usr/include/sys/_types.h" 3 4

struct __darwin_pthread_handler_rec
{
	void           (*__routine)(void *);	
	void           *__arg;			
	struct __darwin_pthread_handler_rec *__next;
};
struct _opaque_pthread_attr_t { long __sig; char __opaque[56]; };
struct _opaque_pthread_cond_t { long __sig; char __opaque[40]; };
struct _opaque_pthread_condattr_t { long __sig; char __opaque[8]; };
struct _opaque_pthread_mutex_t { long __sig; char __opaque[56]; };
struct _opaque_pthread_mutexattr_t { long __sig; char __opaque[8]; };
struct _opaque_pthread_once_t { long __sig; char __opaque[8]; };
struct _opaque_pthread_rwlock_t { long __sig; char __opaque[192]; };
struct _opaque_pthread_rwlockattr_t { long __sig; char __opaque[16]; };
struct _opaque_pthread_t { long __sig; struct __darwin_pthread_handler_rec  *__cleanup_stack; char __opaque[1168]; };







# 93 "/usr/include/sys/_types.h" 3 4

typedef	__int64_t	__darwin_blkcnt_t;	
typedef	__int32_t	__darwin_blksize_t;	
typedef __int32_t	__darwin_dev_t;		
typedef unsigned int	__darwin_fsblkcnt_t;	
typedef unsigned int	__darwin_fsfilcnt_t;	
typedef __uint32_t	__darwin_gid_t;		
typedef __uint32_t	__darwin_id_t;		
typedef __uint64_t	__darwin_ino64_t;	

typedef __darwin_ino64_t __darwin_ino_t;	



typedef __darwin_natural_t __darwin_mach_port_name_t; 
typedef __darwin_mach_port_name_t __darwin_mach_port_t; 
typedef __uint16_t	__darwin_mode_t;	
typedef __int64_t	__darwin_off_t;		
typedef __int32_t	__darwin_pid_t;		
typedef struct _opaque_pthread_attr_t
			__darwin_pthread_attr_t; 
typedef struct _opaque_pthread_cond_t
			__darwin_pthread_cond_t; 
typedef struct _opaque_pthread_condattr_t
			__darwin_pthread_condattr_t; 
typedef unsigned long	__darwin_pthread_key_t;	
typedef struct _opaque_pthread_mutex_t
			__darwin_pthread_mutex_t; 
typedef struct _opaque_pthread_mutexattr_t
			__darwin_pthread_mutexattr_t; 
typedef struct _opaque_pthread_once_t
			__darwin_pthread_once_t; 
typedef struct _opaque_pthread_rwlock_t
			__darwin_pthread_rwlock_t; 
typedef struct _opaque_pthread_rwlockattr_t
			__darwin_pthread_rwlockattr_t; 
typedef struct _opaque_pthread_t
			*__darwin_pthread_t;	
typedef __uint32_t	__darwin_sigset_t;	
typedef __int32_t	__darwin_suseconds_t;	
typedef __uint32_t	__darwin_uid_t;		
typedef __uint32_t	__darwin_useconds_t;	
typedef	unsigned char	__darwin_uuid_t[16];
typedef	char	__darwin_uuid_string_t[37];

# 28 "/usr/include/_types.h" 2 3 4

# 38 "/usr/include/_types.h" 3 4

typedef	int		__darwin_nl_item;
typedef	int		__darwin_wctrans_t;

typedef	__uint32_t	__darwin_wctype_t;

























# 65 "/usr/include/stdio.h" 2 3 4





typedef __darwin_va_list	va_list;




typedef	__darwin_off_t		off_t;




typedef	__darwin_size_t		size_t;






typedef __darwin_off_t		fpos_t;










struct __sbuf {
	unsigned char	*_base;
	int		_size;
};


struct __sFILEX;


# 132 "/usr/include/stdio.h" 3 4
typedef	struct __sFILE {
	unsigned char *_p;	
	int	_r;		
	int	_w;		
	short	_flags;		
	short	_file;		
	struct	__sbuf _bf;	
	int	_lbfsize;	

	
	void	*_cookie;	
	int	(*_close)(void *);
	int	(*_read) (void *, char *, int);
	fpos_t	(*_seek) (void *, fpos_t, int);
	int	(*_write)(void *, __const char *, int);

	
	struct	__sbuf _ub;	
	struct __sFILEX *_extra; 
	int	_ur;		

	
	unsigned char _ubuf[3];	
	unsigned char _nbuf[1];	

	
	struct	__sbuf _lb;	

	
	int	_blksize;	
	fpos_t	_offset;	
} FILE;



extern FILE *__stdinp;
extern FILE *__stdoutp;
extern FILE *__stderrp;









	
# 192 "/usr/include/stdio.h" 3 4


# 205 "/usr/include/stdio.h" 3 4









				










# 234 "/usr/include/stdio.h" 3 4

# 244 "/usr/include/stdio.h" 3 4





void	 clearerr(FILE *);
int	 fclose(FILE *);
int	 feof(FILE *);
int	 ferror(FILE *);
int	 fflush(FILE *);
int	 fgetc(FILE *);
int	 fgetpos(FILE * , fpos_t *);
char	*fgets(char * , int, FILE *);



FILE	*fopen(__const char * , __const char * ) __asm("_" "fopen" );

int	 fprintf(FILE * , __const char * , ...) ;
int	 fputc(int, FILE *);
int	 fputs(__const char * , FILE * ) __asm("_" "fputs" );
size_t	 fread(void * , size_t, size_t, FILE * );
FILE	*freopen(__const char * , __const char * ,
	    FILE * ) __asm("_" "freopen" );
int	 fscanf(FILE * , __const char * , ...) ;
int	 fseek(FILE *, long, int);
int	 fsetpos(FILE *, __const fpos_t *);
long	 ftell(FILE *);
size_t	 fwrite(__const void * , size_t, size_t, FILE * ) __asm("_" "fwrite" );
int	 getc(FILE *);
int	 getchar(void);
char	*gets(char *);

extern __const int sys_nerr;		
extern __const char *__const sys_errlist[];

void	 perror(__const char *);
int	 printf(__const char * , ...) ;
int	 putc(int, FILE *);
int	 putchar(int);
int	 puts(__const char *);
int	 remove(__const char *);
int	 rename (__const char *, __const char *);
void	 rewind(FILE *);
int	 scanf(__const char * , ...) ;
void	 setbuf(FILE * , char * );
int	 setvbuf(FILE * , char * , int, size_t);
int	 sprintf(char * , __const char * , ...) ;
int	 sscanf(__const char * , __const char * , ...) ;
FILE	*tmpfile(void);
char	*tmpnam(char *);
int	 ungetc(int, FILE *);
int	 vfprintf(FILE * , __const char * , va_list) ;
int	 vprintf(__const char * , va_list) ;
int	 vsprintf(char * , __const char * , va_list) ;

int	 asprintf(char **, __const char *, ...) ;
int	 vasprintf(char **, __const char *, va_list) ;










char	*ctermid(char *);

char	*ctermid_r(char *);




FILE	*fdopen(int, __const char *) __asm("_" "fdopen" );


char	*fgetln(FILE *, size_t *);

int	 fileno(FILE *);
void	 flockfile(FILE *);

__const char
	*fmtcheck(__const char *, __const char *);
int	 fpurge(FILE *);

int	 fseeko(FILE *, off_t, int);
off_t	 ftello(FILE *);
int	 ftrylockfile(FILE *);
void	 funlockfile(FILE *);
int	 getc_unlocked(FILE *);
int	 getchar_unlocked(void);

int	 getw(FILE *);

int	 pclose(FILE *);



FILE	*popen(__const char *, __const char *) __asm("_" "popen" );

int	 putc_unlocked(int, FILE *);
int	 putchar_unlocked(int);

int	 putw(int, FILE *);
void	 setbuffer(FILE *, char *, int);
int	 setlinebuf(FILE *);

int	 snprintf(char * , size_t, __const char * , ...) ;
char	*tempnam(__const char *, __const char *) __asm("_" "tempnam" );
int	 vfscanf(FILE * , __const char * , va_list) ;
int	 vscanf(__const char * , va_list) ;
int	 vsnprintf(char * , size_t, __const char * , va_list) ;
int	 vsscanf(__const char * , __const char * , va_list) ;

FILE	*zopen(__const char *, __const char *, int);








FILE	*funopen(__const void *,
		int (*)(void *, char *, int),
		int (*)(void *, __const char *, int),
		fpos_t (*)(void *, fpos_t, int),
		int (*)(void *));










int	__srget(FILE *);
int	__svfscanf(FILE *, __const char *, va_list) ;
int	__swbuf(int, FILE *);



# 402 "/usr/include/stdio.h" 3 4

# 414 "/usr/include/stdio.h" 3 4






























# 1 "/usr/include/secure/_stdio.h" 1 3 4

# 23 "/usr/include/secure/_stdio.h" 3 4









# 1 "/usr/include/secure/_common.h" 1 3 4

# 23 "/usr/include/secure/_common.h" 3 4




# 37 "/usr/include/secure/_common.h" 3 4




# 32 "/usr/include/secure/_stdio.h" 2 3 4










extern int __sprintf_chk (char * , int, size_t,
			  __const char * , ...)
  ;




extern int __snprintf_chk (char * , size_t, int, size_t,
			   __const char * , ...)
  ;




extern int __vsprintf_chk (char * , int, size_t,
			   __const char * , va_list)
  ;




extern int __vsnprintf_chk (char * , size_t, int, size_t,
			    __const char * , va_list)
  ;







# 444 "/usr/include/stdio.h" 2 3 4


# 2 "sqr.c" 2



int main(){
  printf("%i\n",3+2*3+2);
}
