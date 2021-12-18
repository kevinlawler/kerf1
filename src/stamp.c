#include "kerf.h"

#pragma mark - Expiry

/* should return 0 in any pathological cases */
I time_fraction_consumer(const char *s, I n)
{
  I slen=strlen(s);
  char *endptr;
  if ((slen>n) && (s[slen - (1+n)]=='.'))
    {
      return strtol(&s[slen-n],&endptr,10);  
    }
  else 
    {
      return 0;
    }
}

I nanos_xstrptime(const char *s, const char *format)
{
  struct tm t = TM_EPOCH;
  S stop = strptime(s,format,&t);
  bool local = false;
  I nanos = stampI_from_tm(&t,local); 
  /* can get away with this as strptime ignores remainder stuff */
  I len = strlen(format);
  if ((len>3) && ('%'==format[len-2])&& ('.'==format[len-3])) // fake and gay
    {  
      SW(format[len-1])
        {
          CS('N',nanos = nanos + time_fraction_consumer(s,9))
          CS('Q',nanos = nanos + 1000 * time_fraction_consumer(s,6))
          CS('q',nanos = nanos + MILLION * time_fraction_consumer(s,3))
        }      
    }
  return nanos;
}

struct tm tm_from_date_define()
{
  struct tm t = {0};

  S stop = strptime(__DATE__, "%b %d %Y", &t);

  char scan_month[16] = {0};
  int day, year;//unused
  sscanf(__DATE__, "%s %d %d", (S)&scan_month, &day, &year);

  //strptime doesn't cut it here because
  //the compiler uses asctime and the %b locale
  //thing doesn't match up. sigh.
  S months[] = {"Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec", NULL};

  S *look = months;

  while((NULL != *look) && strcmp(scan_month, *look)) look++;

  t.tm_mon = (look - months);

  return t;
}

void expires_binarys_date()
{
  struct tm t = tm_from_date_define();

  char compile_date[256] = {0};
  snprintf((S)&compile_date, sizeof(compile_date), "%04d.%02d.%02d",  1900 + t.tm_year, 1 + t.tm_mon, t.tm_mday);

  //Valid For
  S duration = ""
  //"2 weeks"; t.tm_mday += 14;
  //"20 days"; t.tm_mday += 20;
  //"4 weeks"; t.tm_mday += 28;
  //"1 month";  t.tm_mon += 1;
  "2 months"; t.tm_mon += 2;
  //"3 months"; t.tm_mon += 3;
  //"4 months"; t.tm_mon += 4;
  //

  time_t tmp = timegm(&t);
  t = *gmtime(&tmp);

  char expiry_date[256] = {0};
  strftime((S)&expiry_date, sizeof(expiry_date), "%Y.%m.%d", &t);

  K expiry = ex(&expiry_date); //or use parser methods shrug

  I timestamp = expiry->i;

  rd(expiry);

  S nag = "Contact Kerf at kerf.concerns@gmail.com";

  if(NOW_DATE_ONLY() > timestamp)
  {
    fflush(stdout);
    fprintf(stderr, "Version of Kerf has expired.\n");
    fprintf(stderr, "%s\n", nag);

    goto failed;
  }
  else
  {
    fflush(stdout);
    if(!The_Quiet_Flag && chatty_environment())
    {
      fprintf(stderr, "Binary expires on %s (%s from compilation)\n", expiry_date, duration);
    }
  }

succeeded:
  The_Passed_License_Check_Flag = true;
  return;

failed:
  kerf_exit(1);
}

bool license_at_path_is_good(S license_path)
{
  //obfuscated K map = ex("eval '\\u00ae\\u00af' xor read_from_path($1)",kcv(cwd_license_path));
  K a = read_from_path(kcv(license_path));
  K b = kcv("12");
  kC(b)[0] = 0xae;
  kC(b)[1] = 0xaf;

  K c = xor(a, b);

  K map = monad_eval(c);

  rd(a);
  rd(b);
  rd(c);

  K0 o;
  K expire_date = AT2(map,kcv("expiry"),o);

  //Date must pass
  if(expire_date->i < NOW_DATE_ONLY())
  {
    fprintf(stderr, "License expired.\n");
    goto failed;
  }

  //Checksum must pass
  I magic_number = 82349792832925LL;
  K0 o2;
  K sig = AT2(map,kcv("sign"),o2);
  K sub = ex("{[m] k:['customer', 'expiry']; map(k,m[k])} $1", map);
  update(sub, kcv("kerf"), ki(magic_number));
  K cs  = xhash(sub);
  
  if(cs->i != sig->i)
  {
    fprintf(stderr, "License signature invalid.\n");
    goto failed;
  }

  if(!The_Quiet_Flag && chatty_environment())
  {
    K0 o;
    rd(ex("out 'Licensed Customer: ' # $1 # ' / Expires: ' # rep($2) # '\n'", AT2(map,kcv("customer"),o), expire_date));
  }

  rd(sig);
  rd(sub);
  rd(cs);
  rd(expire_date); // something still leaking here
  rd(map);

succeeded:
  return true;

failed:
  return false;
}

void expires_license_file()
{
  /////////////////////////////////////////////////////

  S filename_only = "kerf-license.dat";

  S cwd_license_path = filename_only;
  C home_license_path[255] = {0};

  struct passwd* pwd = getpwuid(getuid());
  char* home_dir = pwd->pw_dir;
  char* subdir = ".kerf";
  I wanted = snprintf(home_license_path, sizeof(home_license_path), "%s/%s/%s", home_dir, subdir, filename_only);

  fflush(stdout);

  if(wanted >= sizeof(home_license_path))
  {
    fprintf(stderr, "Path to home directory license file too long.\n");
  }

  if(file_exists_valid(cwd_license_path))
  {
    if(license_at_path_is_good(cwd_license_path)) goto succeeded;
  }
  else if(file_exists_valid(home_license_path))
  {
    if(license_at_path_is_good(home_license_path)) goto succeeded;
  }
  else
  {
    fprintf(stderr, "Could not find Kerf license in %s%s or %s. Exiting.\n", "./", cwd_license_path, home_license_path);
    goto failed;
  }

  /////////////////////////////////////////////////////

failed:
  kerf_exit(1);

succeeded:
  The_Passed_License_Check_Flag = true;
  return;
}


void expires()
{
  if(!EXPIRES) return;


  expires_license_file();
}

#pragma mark - Timestamp

I DATE_ONLY(I stamp)
{
  //12:00:00AM current day utc in nanoseconds from unix epoch
  time_t seconds = stamp/BILLION;

  struct tm t = *gmtime(&seconds);

  t.tm_hour = t.tm_min = t.tm_sec = 0;

  seconds = timegm(&t);
  
  return seconds * BILLION;
}

I TIME_ONLY(I stamp)
{
  return stamp - DATE_ONLY(stamp); 
}

I NOW_DATE_ONLY()
{
  return DATE_ONLY(NOW_STAMP());;
}

I NOW_TIME_ONLY()
{
  return TIME_ONLY(NOW_STAMP()); 
}

I NOW_STAMP()//nanoseconds from unix epoch timestamp
{
  I now = 0;

  struct timespec ts;

#ifdef __MACH__
  //POTENTIAL_OPTIMIZATION_POINT
  //do we need to/should we cache any of this OSX timer overhead for accuracy?
  clock_serv_t cclock;
  mach_timespec_t mts;
  host_get_clock_service(mach_host_self(), CALENDAR_CLOCK, &cclock);
  clock_get_time(cclock, &mts);
  mach_port_deallocate(mach_task_self(), cclock);
  ts.tv_sec = mts.tv_sec;
  ts.tv_nsec = mts.tv_nsec;
#else
  clock_gettime(CLOCK_REALTIME, &ts);
#endif

  now = (ts.tv_sec * BILLION) + ts.tv_nsec;

  return now;
}

#pragma mark - Verbs

K now(K x) { return Ks(NOW_STAMP()); }
K now_date(K x) { return Ks(DATE_ONLY(NOW_STAMP())); }
K now_time(K x) { return Ks(TIME_ONLY(NOW_STAMP())); }

K stamp_diff(K x, K y)
{
  //i added this quick to optimize stampvec['time']
  //being a bit fast and loose here since we should handle STAMP[VEC] x STAMP[VEC] and {0,1}xN and Nx{0,1} cases
  //perhaps we should just expand `minus` to do this?
  if(STAMPVEC==xt && STAMPVEC==yt && xn==yn)
  {
    K z = new_k(STAMPVEC, xn);
    work_push(z);
    DO(xn, zI[i] = xI[i] - yI[i])
    work_pop();
    return z;
  }

  if(!IS_STAMP(x) || !IS_STAMP(y)) ERROR(ERROR_TYPE);

  return Ki(xi-yi);
}

#pragma mark - Grade

ALWAYS_INLINE I int_for_TM_NANOS_and_code(TM_NANOS t, C code)
{
    SW(code)
    {
      CD:
      CR('y', 1970 + t.tm_year)
      CR('m', 1 + t.tm_mon)
      CR('d', 1 + t.tm_mday)
      CR('h', t.tm_hour)
      CR('i', t.tm_min)
      CR('s', t.tm_sec)
      CR('n', t.tm_nanos)
      CR('w', t.tm_wday)
      CR('a', t.tm_yday)
    }
}

K array_thing_bucket_nanos(I *table, K dtbucket, TM_NANOS abs_tm, bool swap)
{

  I n = ECOUNT(dtbucket);
  K x = new_k(INTVEC, n);
  K y = new_k(INTVEC, n);

  ENUM(dtbucket,

    I k = 0;
    assert(ut == CHARVEC);
    assert(vt == INT);//we could generalize but haven't

    k = int_for_TM_NANOS_and_code(abs_tm, uC[0]);

    xI[i] = si;
    yI[i] = k;
  )

  if(swap)
  {
    SWAP(x,y);
  }

  K z = tri_compare(x,y,table); 
  rd(x);rd(y);
  return z;
}

#pragma mark - Conversion

char week_of_year_from_TM_NANOS(TM_NANOS t)
{
  char week_is_one_indexed = 1;
  return ((t.tm_yday + 7 - (t.tm_wday ? (t.tm_wday - 1) : 6)) / 7) + week_is_one_indexed;
}

TM_NANOS TM_NANOS_from_dtbucket_and_initializer(K x, TM_NANOS i)
{
  TM_NANOS n = i;

  K z = NULL;
  K0 o;

  if((z = LOOKUP_(x, kcv("y"), o)))
  {
    n.tm_year = INTIFY(z);
  }

  if((z = LOOKUP_(x, kcv("m"), o)))
  {
    n.tm_mon = INTIFY(z);
  }

  if((z = LOOKUP_(x, kcv("d"), o)))
  {
    n.tm_mday = INTIFY(z);
  }

  if((z = LOOKUP_(x, kcv("h"), o)))
  {
    n.tm_hour = INTIFY(z);
  }

  if((z = LOOKUP_(x, kcv("i"), o)))
  {
    n.tm_min = INTIFY(z);
  }

  if((z = LOOKUP_(x, kcv("s"), o)))
  {
    n.tm_sec = INTIFY(z);
  }

  //Note: While you can reduce say 2014y13m to 2015y01m
  //      in general this does not work, because
  //      individual datetime components do not translate.
  //      There is no fixed number of days in a month,
  //      or seconds in a minute.
  //      Avoid going down this road.


  return n;
}

TM_NANOS TM_NANOS_from_stampI(I i, bool local)
{
  TM_NANOS n = TM_NANOS_ZEROES;

  time_t seconds = i/BILLION;
  I  nanoseconds = i - (seconds * BILLION);

  struct tm t = {0};
  if(local) t = *localtime(&seconds);
  else      t =    *gmtime(&seconds);

  n.tm_year  = -70 + t.tm_year;
  n.tm_mon   = t.tm_mon;
  n.tm_mday  = -1 + t.tm_mday;
  n.tm_hour  = t.tm_hour;
  n.tm_min   = t.tm_min;
  n.tm_sec   = t.tm_sec;

  n.tm_nanos = nanoseconds;

  n.tm_wday  = t.tm_wday;
  n.tm_yday  = t.tm_yday;
  n.tm_isdst = t.tm_isdst;

  return n;
}

TM_NANOS TM_NANOS_from_stamp(K x, bool local)
{
  return TM_NANOS_from_stampI(xi, local);
}


TM_NANOS TM_NANOS_for_unit(C unit, I i)
{
  TM_NANOS z = TM_NANOS_ZEROES;

  SW(unit)
  {
    CS('y', z.tm_year  += i)
    CS('m', z.tm_mon   += i)
    CS('d', z.tm_mday  += i)
    CS('h', z.tm_hour  += i)
    CS('i', z.tm_min   += i)
    CS('s', z.tm_sec   += i)
    CS('n', z.tm_nanos += i)
    CS('w', z.tm_wday  += i)
    CS('a', z.tm_yday  += i)
  }

  return z;
}

TM_NANOS TM_NANOS_affine(TM_NANOS x, C unit, I interval, I multiple)
{
  TM_NANOS y = TM_NANOS_for_unit(unit, interval * multiple);
  return TM_NANOS_sum(x, y);
}

TM_NANOS TM_NANOS_sum(TM_NANOS x, TM_NANOS y)
{
  TM_NANOS z = TM_NANOS_ZEROES;

  z.tm_year  = x.tm_year  + y.tm_year;
  z.tm_mon   = x.tm_mon   + y.tm_mon;
  z.tm_mday  = x.tm_mday  + y.tm_mday;
  z.tm_hour  = x.tm_hour  + y.tm_hour;
  z.tm_min   = x.tm_min   + y.tm_min;
  z.tm_sec   = x.tm_sec   + y.tm_sec;

  z.tm_nanos = x.tm_nanos + y.tm_nanos;

  z.tm_wday  = x.tm_wday  + y.tm_wday; 
  z.tm_yday  = x.tm_yday  + y.tm_yday;
  //z.tm_isdst = x.tm_isdst + y.tm_isdst

  return z;
}

TM_NANOS TM_NANOS_from_tm(void *s)
{

  struct tm *t = (struct tm *)s;

  TM_NANOS x = TM_NANOS_ZEROES;

  x.tm_year   = -70 + t->tm_year;
  x.tm_mon    = t->tm_mon;
  x.tm_mday   = -1 + t->tm_mday;
  x.tm_hour   = t->tm_hour;
  x.tm_min    = t->tm_min;
  x.tm_sec    = t->tm_sec;

  x.tm_wday   = t->tm_wday;
  x.tm_yday   = t->tm_yday;
  x.tm_isdst  = t->tm_isdst;

  return x;
}

I stampI_from_tm(void *s, bool local)
{
  TM_NANOS x = TM_NANOS_from_tm(s);
  return stampI_from_TM_NANOS(x, local);
}

I stampI_from_TM_NANOS(TM_NANOS x, bool local)
{
  volatile bool use_local = local; //sic. fix "-flto -fuse-linker-plugin" compile bug as of 2016.01.08

  struct tm t = TM_EPOCH;

  t.tm_year   = 70 + x.tm_year;
  t.tm_mon    = x.tm_mon;
  t.tm_mday   = 1 + x.tm_mday;
  t.tm_hour   = x.tm_hour;
  t.tm_min    = x.tm_min;
  t.tm_sec    = x.tm_sec;

  t.tm_wday   = x.tm_wday;
  t.tm_yday   = x.tm_yday;
  t.tm_isdst  = x.tm_isdst;

  time_t unixtime = 0;
  
  if(use_local)
  {
    unixtime = timelocal(&t);
  }
  else
  {
    unixtime = timegm(&t);
  }

  I stamp = (unixtime * BILLION) + x.tm_nanos; 

  return stamp;
}

I time_format_chunk(S fbuf, I dst, F seconds_frac, S fmt) {
  // this will write an unnecessary leading '0.'
  // and a (harmless) trailing \0:
  I len = sprintf(fbuf + dst, fmt, seconds_frac);

  // in-place shift left by two to trim leading junk:
  for(I off = 0; off < len - 2; off++)
  {
    fbuf[dst + off] = fbuf[dst + off + 2];
  }
  fbuf[dst + (len - 2)] = '\0';

  // report written chars:
  return dst + (len - 2);
}

stampbuf time_format(K x, bool local, S format) {
  // We want to do as little of this on our own as possible,
  // So I piggyback on strftime. We add a meaning for
  // Q and q, the only unpopulated format letters.

  // calculate the components of the stamp.
  // logic unfeelingly stolen from time_from_stamp():
  time_t seconds = xi/BILLION;
  struct tm t = (local) ? *localtime(&seconds) : *gmtime(&seconds);
  I nanos = ABS((xi-(seconds*BILLION)));
  F seconds_frac = (nanos/((F)BILLION));

  // expand our custom format sequences:
  I len = strlen(format);
  I dst = 0;
  char fbuf[4096];

  for(I src = 0; src < len; src++)
  {
    if (format[src] == '%' && format[src+1] == 'q')
    {
      dst = time_format_chunk((char*)&fbuf, dst, seconds_frac, "%1.3f");
      src++;
    }
    else if (format[src] == '%' && format[src+1] == 'Q')
    {
      dst = time_format_chunk((char*)&fbuf, dst, seconds_frac, "%1.6f");
      src++;
    }
    else if (format[src] == '%' && format[src+1] == 'N')
    {
      dst = time_format_chunk((char*)&fbuf, dst, seconds_frac, "%1.9f");
      src++;
    }
    else 
    {
      fbuf[dst++] = format[src];
    }
  }
  fbuf[dst] = '\0';

  // pass the buck:
  stampbuf buf;
  strftime((S)&buf, sizeof(buf), (char*)&fbuf, &t);
  return buf;
}

#pragma mark - Puts

stampbuf time_from_stamp(K x, bool local, int decimal_places, I no_rollover)
{
  struct tm t;

  time_t seconds = xi/BILLION;

  I rollover_threshold = 24*60*60 - 1;
  I overage = 0;

  if (seconds > rollover_threshold) 
  {
    overage = seconds-rollover_threshold;
  }

  SW(overage)
  {
    CS(0,)   //regular
    CS(1,)   //leap second
    CD:break;//more than a leap second?
  }

  if (no_rollover)//capture leap second
  {
    seconds -= overage;
  }

  if(local) t = *localtime(&seconds);
  else      t =    *gmtime(&seconds);

  if (no_rollover)
  {
    t.tm_sec += overage;
  }

  stampbuf buf;
  stampbuf buf2 = {0};
  stampbuf combined;

  S format = "%H:%M:%S";
  
  strftime((S)&buf, sizeof(buf), format, &t); 

  I round = false; //rounding timestamps is a bad idea - can change second, day, month, etc.
  round = round;   //suppress compiler error. but see above - rounding is a biiig headache
  I nanos = ABS((xi-(seconds*BILLION)));
  F seconds_frac = (nanos/((F)BILLION));

  if(!round) //truncate
  {
    F power = pow(10,decimal_places);
    I decimal = power*(seconds_frac);
    seconds_frac = decimal / power;
  }

  if(0<decimal_places)
  {
    snprintf((S)&buf2, sizeof(buf2), "%1.*f", (int)decimal_places, seconds_frac); //0.0123..., variable
    S s = (S)&buf2;
    while((s[0] = s[1]))s++;//move left over zero
  }

  //Linux seems to need this combined, probably because it can't
  //snprintf with the same src and dest (like memmov can) 
  snprintf((S)&combined, sizeof(combined), "%s%s", (S)&buf, (S)&buf2);
  
  return combined;
}

stampbuf date_from_stamp(K x, bool local)
{
  struct tm t;

  time_t seconds = xi/BILLION;

  if(local) t = *localtime(&seconds);
  else      t =    *gmtime(&seconds);

  stampbuf buf;
  S format = "%Y.%m.%d";
  
  strftime((S)&buf, sizeof(buf), format, &t); 
  
  return buf;
}

stampbuf datetime_from_stamp(K x, bool local, int decimal_places)
{
  stampbuf a = date_from_stamp(x,local);
  stampbuf b = time_from_stamp(x,local, decimal_places, false);
  stampbuf c;
  snprintf((S)&c,sizeof(c), "%sT%s", (S)&a,(S)&b);
 
  return c;
}

bool has_date_only(K x)
{
  time_t s = xi/BILLION;

  I nanos = xi - s*BILLION;

  if (0 != nanos) return false;

  struct tm t = *gmtime(&s);

  if (0 != t.tm_sec || 0 != t.tm_min || 0 != t.tm_hour) return false;

  return true;
}

bool has_time_only(K x)
{
  SW(xt)
  {
    CS(STAMP,    if(xi >= TIME_DATE_CUTOFF) return false)
    CS(STAMPVEC, DO(xn, if(xI[i] >= TIME_DATE_CUTOFF) return false))
    CD: return false;
  }

  return true;
}

K format_stamp(K x,K y)
{
  if (IS_VLIST(y))
  {
    K z = Kk();
    work_push(z);
    ENUM(y, K e = format_stamp(x, u); z = cow_add(z, e); rd(e));
    work_pop_rd(false);
    return z;
  }

  if (!IS_STRING(x) || !IS_STAMP(y)) { ERROR(ERROR_TYPE); }

  x = cow_ensure_null_terminated_chars(strong(x));
  stampbuf r = time_format(y, false, xg);
  rd(x);
  return charvec_from_cstring((S)&r);
}

S parse_stamp_ns(F* fraction, S source, I digits)
{
  F n = 0;
  F b = .1;
  for(I x = 0; x < digits; x++)
  {
    if (!source || !isdigit(source[0])) { break; }
    n += b * (source[0] - '0');
    b /= 10.0;
    source++;
  }
  *fraction = n;
  return source;
}

S parse_stamp_chunk(struct tm* sofar, F* fraction, S source, C token)
{
  if (token == 'N')
  {
    // parse nanoseconds (9 digits)
    return parse_stamp_ns(fraction, source, 9);
  }
  if (token == 'q')
  {
    // parse milliseconds (3 digits)
    return parse_stamp_ns(fraction, source, 3);
  }
  if (token =='Q')
  {
    // parse microseconds (6 digits)
    return parse_stamp_ns(fraction,source,6);
  }
  // pass the buck to strptime:
  char format[3];
  format[0] = '%';
  format[1] = token;
  format[2] = '\0';
  return strptime(source, format, sofar);
}

K parse_stamp(K x, K y)
{
  // parse_stamp is right-atomic:
  if (IS_VLIST(y) && !IS_STRING(y))
  {
    K z = Kk();
    work_push(z);
    ENUM(y, K e = parse_stamp(x, u);work_pop(); z = cow_add(z, e);work_push(z); rd(e));
    work_pop_rd(false);
    return z;
  }

  // x is a format string, y is a string to parse:
  if (!IS_STRING(x) || !IS_STRING(y)) { ERROR(ERROR_TYPE); }
  x = cow_ensure_null_terminated_chars(strong(x));
  y = cow_ensure_null_terminated_chars(strong(y));

  // process the input a token at a time:
  struct tm t = TM_EPOCH;
  F fraction = 0;
  S source = yC;
  for(I index = 0; (index < xn) && (source != NULL); index++)
  {
    if (xC[index] == '%')
    {
      // process a token
      index++;
      source = parse_stamp_chunk(&t, &fraction, source, xC[index]);
    }
    else
    {
      // skip literal characters
      source++;
    }
  }

  // convert unix time + fractional time to a Kerf timestamp (assume gmt):
  time_t unixtime = timegm(&t);
  I nanos = (unixtime * BILLION) + (I)(fraction * BILLION);
  K r = Ks(nanos);

  rd(x);
  rd(y);
  return r;
}

stampbuf buf_from_stamp(K x, bool local, int decimal_places)
{
  // if we have a custom stamp format configured, it overrides
  // all other notions of prettyprinting:
  K format = ex(".Print.stamp_format");
  if(IS_STRING(format))
  {
    // mash that sucker into a C string
    C stamp_format[1024];
    I length = MIN(sizeof(stamp_format), 1+COUNT(format));
    snprintf(stamp_format, length, "%s", kC(format));
    rd(format);

    // do our dirty work:
    return time_format(x, local, stamp_format);
  }
  rd(format);

  //23:25:43.877
  //2014.07.03
  //2014.07.03T23:25:43.877
  if(has_time_only(x)) return time_from_stamp(x,local,decimal_places,true);
  if(has_date_only(x)) return date_from_stamp(x,local);
  return datetime_from_stamp(x,local,decimal_places);
}

#pragma mark - Parse

K parse_absolute_date(K x, bool local)
{

  //POTENTIAL_OPTIMIZATION_POINT
  //auto memory buffer
  K y = copy(x);
  y = cow_ensure_null_terminated_chars(y);


  //yC[4]=' ';//set to spaces to allow both 2016.01.01 and 2016-01-01
  //yC[7]=' ';
  DO(yn, if(yC[i]=='-')yC[i]='.')

  struct tm t = {0};
  S stop = strptime(yC, "%Y.%m.%d", &t);

  time_t unixtime = 0;
  
  if(local)
  {
    unixtime = timelocal(&t);
  }
  else
  {
    unixtime = timegm(&t);
  }

  I nanos = unixtime * BILLION; 
  K stamp = Ks(nanos);

  rd(y);

  return stamp;
}

K parse_absolute_time(K x, bool local)
{
  K y = copy(x);
  y = cow_ensure_null_terminated_chars(y);

  struct tm t = TM_EPOCH;
  S stop = strptime(yC, "%H:%M:%S", &t);

  stop = NULL;//linux borks this for some reason so we can't use

  time_t unixtime = 0;
  
  if(local)
  {
    unixtime = timelocal(&t);
  }
  else
  {
    unixtime = timegm(&t);
  }

  //Capture decimal in 12:34:56.123
  //If strptime has built-in support for this, I 
  //couldn't find it in 15 minutes.
  F fraction = 0;

  I k = -1;

  DO(yn, if(yC[i]=='.'){k = i; break;}) 
  if(k >= 0)//linux borked our stop method
  //if(stop && '.'==*stop)
  {
    fraction = strtod(yC + k, 0);
    //fraction = strtod(stop, 0);
  }

  I nanos = (unixtime * BILLION) + (I)(fraction * BILLION); 

  K stamp = Ks(nanos);

  rd(y);

  return stamp;
}

K parse_absolute_datetime(K x, bool local)
{
  //POTENTIAL_OPTIMIZATION_POINT
  //do as one

  K list = explode(kc('T'), x);

  K date = parse_absolute_date(kN(list,0), local);
  K time = parse_absolute_time(kN(list,1), local);

  K datetime = Ks(date->i + time->i);

  rd(date);
  rd(time);
  rd(list);

  return datetime;
}

K parse_relative_datetime(K x, bool local)
{
  K y = copy(x);
  y = cow_ensure_null_terminated_chars(y);

  //uniform separator: replace - by . 
  DO(COUNT(y), if('-'==yC[i]) yC[i]='.')

  //for uniform chars case
  DO(COUNT(y), yC[i] = tolower(yC[i]))

  //separate smushed 2015Y01M to 2015Y.01M via '.'
  K z = new_k(CHARVEC, 0);
  DO(xn, C c = yC[i];

    z = cow_add(z, kc(c));

    bool smushed = isalpha(c) && i < xn - 1 && yC[i+1] != '.';

    if(smushed)
    {
      z = cow_add(z, kc('.'));
    }
  )

  K list = explode(kc('.'), z);

  K bucket = new_map();
  SET_ALT_ATTR(bucket, MAP_ATTR_DTBUCKET);

  ENUM(list,

    K numerals = copy(v);
    numerals->n -= 1;

    K number = parse_number(numerals);

    K key = new_k(CHARVEC,1);
    kC(key)[0] = kC(v)[vn-1];

    bucket = update(bucket, key, number);

    rd(numerals);
    rd(key);
    rd(number);
  )

  rd(y);
  rd(z);
  rd(list);

  return bucket;
}

#pragma mark - Bars & Time Indexing

void bars_update_bucket_bin_search (I left_multiple, I right_multiple, TM_NANOS *left_bucket, TM_NANOS *right_bucket, I *left_stamp, I *right_stamp, I target_stamp, C units_code, I units_interval, bool local)
{
  //assert: on 1st run: left_bucket <=  target_stamp  < right_bucket

  //Success: we've found the adjacent bounding buckets
  if(right_multiple <= left_multiple + 1)
  {
    return;
  }

  //Find midpoint
  I middle_multiple = (right_multiple - left_multiple)/2 + left_multiple;  //sic, overflow
  TM_NANOS middle_bucket = TM_NANOS_affine(*left_bucket, units_code, units_interval, middle_multiple - left_multiple);
  I middle_stamp = stampI_from_TM_NANOS(middle_bucket, local);

  //Then either go left or go right
  if(middle_stamp > target_stamp)//Need to go left
  {
    right_multiple = middle_multiple;
    *right_bucket  = middle_bucket;
    *right_stamp   = middle_stamp;
  }
  else //Need to go right
  {
    left_multiple = middle_multiple;
    *left_bucket  = middle_bucket;
    *left_stamp   = middle_stamp;
  }

  bars_update_bucket_bin_search(left_multiple, right_multiple, left_bucket, right_bucket, left_stamp, right_stamp, target_stamp, units_code, units_interval, local);
}

void bars_update_bounding_buckets_for_stamp(TM_NANOS *left_bucket, TM_NANOS *right_bucket, I *left_stamp, I *right_stamp, I target_stamp, C units_code, I units_interval, bool local)
{
  //POTENTIAL_OPTIMIZATION_POINT
  //we can guess the next bucket by keeping track of previous multiples

  I bound = 30; // `struct tm` only holds regular `int` values

  //geometric search:
  //increase until greater
  //then use binary search on the last range 
  I left_multiple  = 0; 
  I right_multiple = 0;

  TM_NANOS starting_bucket; 
  
  //POTENTIAL_OPTIMIZATION_POINT
  //units codes less than 1s (eg, 1microsecond) we don't need to roundtrip stampI_from_TM_NANOS
  //we can add to the stamps directly. here and in the binary search that follows

  //This will look in both directions, up or down for the target stamp
  //While we could factor this, I think that would make it less clear
  if(*left_stamp > target_stamp)   // target < left_stamp < right_stamp 
  {
    starting_bucket = *left_bucket;

    left_multiple = -1;

    //decrease left_stamp until less
    do
    {
      *right_bucket = *left_bucket;
      *right_stamp  = *left_stamp;

      *left_bucket = TM_NANOS_affine(starting_bucket, units_code, units_interval, left_multiple);
      *left_stamp = stampI_from_TM_NANOS(*left_bucket, local);

      if(*left_stamp <= target_stamp) break;
      left_multiple *= 2;

    } while(ABS(left_multiple) < POW2(bound));

    right_multiple = left_multiple / 2;
  }
  else                             // left_stamp < right_stamp < target
  {
    starting_bucket = *right_bucket;

    right_multiple = 1;

    //increase right_stamp until greater
    do
    {
      *left_bucket = *right_bucket;
      *left_stamp  = *right_stamp;

      *right_bucket = TM_NANOS_affine(starting_bucket, units_code, units_interval, right_multiple);
      *right_stamp = stampI_from_TM_NANOS(*right_bucket, local);

      if(target_stamp < *right_stamp) break;
      right_multiple *= 2;

    } while(ABS(right_multiple) < POW2(bound));

    left_multiple = right_multiple / 2;
  }

  bool checks_bound = true;
  if(checks_bound && right_multiple >= POW2(bound))
  {
    fprintf(stderr, "Error: grouping time value too far apart.\n");
    ERROR(ERROR_TIME);
  }

  //binary search last range 
  bars_update_bucket_bin_search(left_multiple, right_multiple, left_bucket, right_bucket, left_stamp, right_stamp, target_stamp, units_code, units_interval, local);
}

K bars(K x, K y, K z)
{
  I n = cy;

  if(0==n) return strong(y);

  if(!IS_DTBUCKET(x) || STAMPVEC != -ayt)
  {
    return ex("$1 * floor $2/$1", x, y);
  }

  assert(IS_DTBUCKET(x) && STAMPVEC == -ayt);

  if(lenI(xKeys) != 1)
  {
    fprintf(stderr, "Error: Relative time argument must only consist of one time unit.\n");
    ERROR(ERROR_ARGS);
  }

  K0 o1,o2;

  K key   = LOOK_(xKeys, 0, o1);
  K value = LOOK_(xValues, 0, o2);

  C code     = kC(key)[0];
  I interval = value->i;

  I reference_stamp = 0;//could use something else

  bool local = false;
  I *iy = PAYLOAD_START(y);

  if(z && STAMP == zt)
  {
    reference_stamp = zi;

    TM_NANOS bucket = TM_NANOS_from_stampI(reference_stamp, local);

    SW(code) //fallthrough all, note offset differs here from other version
    {      
      CSF('y', bucket.tm_mon   = 0)
      CSF('m', bucket.tm_mday  = 0)
      CSF('d', bucket.tm_hour  = 0)
      CSF('h', bucket.tm_min   = 0)
      CSF('i', bucket.tm_sec   = 0)
      CSF('s', bucket.tm_nanos = 0)
    //CSF('w', bucket.tm_wday  = 0)
    //CSF('a', bucket.tm_yday  = 0)
    }

       reference_stamp = stampI_from_TM_NANOS(bucket, local);
  }
  else
  {
    reference_stamp = iy[0]; //first item

    //K k = xmin(y, NULL);   //min item
    //reference_stamp = k->i;    
    //rd(k);

    TM_NANOS bucket = TM_NANOS_from_stampI(reference_stamp, local);

    int wday = bucket.tm_wday;

    SW(code) //fallthrough all, note offset differs here from other version
    {
      CSF('y', bucket.tm_year  = 0)
      CSF('m', bucket.tm_mon   = 0)
      CSF('d', bucket.tm_mday  = 0)
      CSF('h', bucket.tm_hour  = 0)
      CSF('i', bucket.tm_min   = 0)
      CSF('s', bucket.tm_sec   = 0)
      CSF('n', bucket.tm_nanos = 0)
    //CSF('w', bucket.tm_wday  = 0)
    //CSF('a', bucket.tm_yday  = 0)
    }

    if('d'==code && 7==interval) //adjust 7d==week buckets to start with Sunday
    {
      bucket.tm_mday -= wday + 1; 
    }

    reference_stamp = stampI_from_TM_NANOS(bucket, local);
  }

  I current_stamp = reference_stamp;
  TM_NANOS current_bucket = TM_NANOS_from_stampI(current_stamp, local);

  I next_stamp = current_stamp;
  TM_NANOS next_bucket = TM_NANOS_from_stampI(next_stamp, local);

  K w = new_k(yt, n);
  I *iw = PAYLOAD_START(w);

  DO(n, I target_stamp = iy[i]; 

        if(current_stamp <= target_stamp && target_stamp < next_stamp)
        {
          iw[i] = current_stamp;
        }
        else
        {
          //POTENTIAL_OPTIMIZATION_POINT
          //we can pre-generate / cache likely bucket-stamps. then bin search them for times
          //you can find the min and max time in the stamp vector, determine how many intervals they're separated by
          //then generate all the buckets in between. then bin search them. (if the range is small enough, <10M say)

          //Find bracketing buckets for stamp
          bars_update_bounding_buckets_for_stamp(&current_bucket, &next_bucket, &current_stamp, &next_stamp, target_stamp, code, interval, local);
          //Buckets and stamps have been updated.
          assert(current_stamp <= target_stamp); 
          assert(target_stamp < next_stamp);

          iw[i] = current_stamp;


          continue;
        }
  )

  return w;
}

ALWAYS_INLINE K bar_subsecond_modular_with_divisor(K x, I divisor)
{
  //Given a timestamp vector, return 'millisecond' vector or ...

  K y = ex("$1['time']", x);

  y = cow(y);
  yt = INTVEC;

  work_push(y);

  DO(yn, I seconds = yI[i]/BILLION; 
         I nanos   = yI[i]-seconds*BILLION;  
         yI[i]     = nanos / divisor;
  )

  work_pop();

  return y;
}

ALWAYS_INLINE K bar_modular_time_index_by_unit_code(K x, C code)
{
  //POTENTIAL_OPTIMIZATION_POINT
  //We can optimize this further by copy-pasta'ing the bars function
  //and putting our calculation inside it, instead of what we're doing
  //here (which is extra TM_NANOS calculations and so on)

  if(0==xn) return new_k(INTVEC, 0);

  bool local = false; 

  K relative = NULL;

  SW(code)
  {
    CD:
    CS('y', relative = ex("1y"))
    CS('m', relative = ex("1m"))
    CS('d', relative = ex("1d"))
    CS('h', relative = ex("1h"))
    CS('i', relative = ex("1i"))
    CS('s', relative = ex("1s"))
  }

  work_push(relative);
  K y = ex("bars($1, $2)", relative, x);
  work_pop_rd(true);
  y = cow(y);
  yt = INTVEC;
  work_push(y);

  I cached_stamp = yI[0];
  TM_NANOS cached_bucket = TM_NANOS_from_stampI(cached_stamp, local);

  DO(yn, 
          if(cached_stamp != yI[i])
          {
            cached_stamp = yI[i];
            cached_bucket = TM_NANOS_from_stampI(cached_stamp, local);
          }

          yI[i] = int_for_TM_NANOS_and_code(cached_bucket, code); 
  )

  work_pop_n_rd(1, false);
  return y;

}

#pragma mark - 

int time_go()
{
  return 0;
  I n = 12;
  K x = new_k(-STAMP,n);
  DO(n, xI[i] = NOW_STAMP());
  //TIME(ENUM(x,show(v)))

  show(ks(NOW_STAMP()));
  show(ks(NOW_DATE_ONLY()));
  show(ks(NOW_TIME_ONLY()));
  show(ks(0));
  rd(x);

  return 0;
}


