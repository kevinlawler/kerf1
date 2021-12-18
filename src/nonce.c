#include "kerf.h"

#pragma mark - Pseudo-Random Number Generator
//BSD-style License. Takuji Nishimura & Makoto Matsumoto http://www.math.hiroshima-u.ac.jp/~m-mat/MT/emt.html
//Before using, initialize the state by using init_genrand64(seed)  
//mt19937-64.c, abridged

#define NN 312
#define MM 156
#define MATRIX_A 0xB5026F5AA96619E9ULL
#define UM 0xFFFFFFFF80000000ULL // Most significant 33 bits 
#define LM 0x7FFFFFFFULL // Least significant 31 bits 

static unsigned long long mt[NN]; // The array for the state vector 
static int mti=NN+1; // mti==NN+1 means mt[NN] is not initialized 

void init_genrand64(unsigned long long seed) // initializes mt[NN] with a seed 
{
    mt[0] = seed;
    for (mti=1; mti<NN; mti++) 
        mt[mti] =  (6364136223846793005ULL * (mt[mti-1] ^ (mt[mti-1] >> 62)) + mti);
}

// generates a random number on [0, 2^64-1]-interval
unsigned long long genrand64_int64(void)
{
    int i;
    unsigned long long x;
    static unsigned long long mag01[2]={0ULL, MATRIX_A};

    if (mti >= NN) { // generate NN words at one time 

        // if init_genrand64() has not been called,
        // a default initial seed is used    
        if (mti == NN+1) 
            init_genrand64(5489ULL); 

        for (i=0;i<NN-MM;i++) {
            x = (mt[i]&UM)|(mt[i+1]&LM);
            mt[i] = mt[i+MM] ^ (x>>1) ^ mag01[(int)(x&1ULL)];
        }
        for (;i<NN-1;i++) {
            x = (mt[i]&UM)|(mt[i+1]&LM);
            mt[i] = mt[i+(MM-NN)] ^ (x>>1) ^ mag01[(int)(x&1ULL)];
        }
        x = (mt[NN-1]&UM)|(mt[0]&LM);
        mt[NN-1] = mt[MM-1] ^ (x>>1) ^ mag01[(int)(x&1ULL)];

        mti = 0;
    }
  
    x = mt[mti++];

    x ^= (x >> 29) & 0x5555555555555555ULL;
    x ^= (x << 17) & 0x71D67FFFEDA60000ULL;
    x ^= (x << 37) & 0xFFF7EEE000000000ULL;
    x ^= (x >> 43);

    return x;
}

// generates a random number on [0,1)-real-interval, 53-bit precision
double genrand64_real2(void) { return (genrand64_int64() >> 11) * (1.0/9007199254740992.0); }

#pragma mark -

I SEED;

void random_init()
{
  I seed = 0x0011223344556677LL;

  if(PRNG_USES_RANDOMIZED_SEED_ON_INIT && !IS_DEFINED(DEBUG)) 
  {
    seed = urandomBits();
  }

  seedPRNG(seed);
}

I rI(){R genrand64_int64();}
F rF(){R genrand64_real2();}

I urandomBits()
{
  I s;
  I f=open("/dev/urandom",0);
  I r=read(f,&s,sizeof(s));
  if(!r)
  {
    kerf_exit(EX_OSERR);
  }
  close(f);
  R s;
}

void seedPRNG(I s)
{
  SEED=s;
  init_genrand64(SEED);
}

K verb_seed_prng(K x)
{
  I i = INTIFY(x);
  seedPRNG(i);
  return Ki(1);
}

#pragma mark - Randomization Algorithms

void shuffle_intvec_in_place(K x)//Knuth Algorithm 3.4.2P
{
  I j = 0, k = 0, tmp = 0;
  I n = COUNT(x);;

  //POTENTIAL_OPTIMIZATION_POINT
  //For more optimizations see Knuth Solution 3.4.2-8 (e.g. exploit > 1/2*N symmetry)
  for(j = n - 1; j > 0; j--)
  {
    k = (1 + j) * rF();
    tmp = xI[j]; //swap
    xI[j] = xI[k];
    xI[k] = tmp;
  }
}

K verb_rand(K x, K y)
{
  if(!y)
  {
    SW(xt)
    {
      CS(INT,   return Ki(xi*rF()))
      CS(FLOAT, return Kf(xf*rF()))

     CSF(NIL,)
      CD:
          if(IS_ARRAY(x))
          {
            K0 o;
            return strong(LOOK_(x, rF()*cx,o));  
          }
          return Kf(1.0*rF());
    }
  }

  if(INT != axt && FLOAT != xt && CHAR != xt) ERROR(ERROR_TYPE);

  I c = INTIFY(x);
  I n = ABS(c);
  K indices = NULL;

  SW(yt)
  {
   CSF(CHAR,) //can reclaim 
    CS(INT, I d = INTIFY(y);
            if(c < 0)ERROR(ERROR_SIGN);
            if(d < 0)ERROR(ERROR_SIGN); //Extension: minus int/float could draw from [-x, x)

            //if(0 == yi) return verb_rand(x, kf(1.0));//optional
            K z = new_k(INTVEC, n);
            DO(n, zI[i]=d*rF()) //Note: [0,1) floating-point has fewer bits than I (mantissa only)
            return z;
    )

    CS(FLOAT, F f = yf; 
              if(0 == f) f = 1;
              K z = new_k(FLOATVEC, n);
              DO(n, zF[i]=rF()*f)
              return z;
    )

    CS(MAP,   return verb_rand(x, yKeys))
    CS(TABLE, return verb_rand(x, yKeys)) //possibly it is more useful to sample rows

   CSF(STAMP,)//could reclaim these
   CSF(FUNC,)
   CSF(NIL,)
    CD: if(IS_ATOM(y)) return take(x,y);
        //Arrays:
        indices = verb_rand(x, ki(COUNT(y)));
        work_push(indices);
        //POTENTIAL_OPTIMIZATION_POINT:
        //retrieve values directly
        K z = at(y, indices);
        work_pop_rd(true);
        return z;
        break;
  }
}

K verb_deal(K x, K y)
{
  if(INT != axt && FLOAT != xt && CHAR != xt) ERROR(ERROR_TYPE);

  I c = INTIFY(x);
  I n = ABS(c);
  K indices = NULL;

  if(!y) return verb_deal(x, ki(n));

  SW(yt)
  {
   CSF(CHAR,) //can reclaim 
    CS(INT, I d = INTIFY(y);
            if(c < 0)ERROR(ERROR_SIGN);
            if(d < 0)ERROR(ERROR_SIGN);
            if(d < n)ERROR(ERROR_LENGTH); //Extension: we could do repeats if we wanted

            //if(0 == yi) return verb_rand(x, kf(1.0));//optional

            //potential_feature_point
            //we can allow numbers less than -n by merely starting over
            //with a fresh deck to deal
            K z = new_k(INTVEC, n);
            vitter(zI, zn, d);
            shuffle_intvec_in_place(z);
            return z;
    )

    CS(FLOAT, return verb_deal(x, ki(yf)))
    CS(MAP,   return verb_deal(x, yKeys))
    CS(TABLE, return ex("$2[deal($1,range(len($2)))]",x,y)) // shuffle rows
   CSF(STAMP,)//could reclaim these
   CSF(FUNC,)
   CSF(NIL,)
    CD: if(IS_ATOM(y)) return take(x,y);
        //Arrays:
        indices = verb_deal(x, ki(COUNT(y)));
        work_push(indices);
        //POTENTIAL_OPTIMIZATION_POINT:
        //retrieve values directly
        K z = at(y, indices);
        work_pop_rd(true);
        return z;
        break;
  }
}

void vitter_a(I *a,I n,I N,I j) //Method A
{
  I S,i=0; 
  F top=N-n, Nreal=N, V, quot;
  while(n >= 2)
  {
    V = rF(); S=0; quot=top/Nreal;
    while (quot>V)
    {
      S++; top--; Nreal--;
      quot = (quot * top)/Nreal;
    }
    j+=S+1; 
    a[i++]=j;
    Nreal--; n--;
  }
  S = floor(round(Nreal) * rF());
  j+=S+1;
  a[i++]=j;
}

//POTENTIAL_OPTIMIZATION_POINT: Christian Neukirchen points out we can replace exp(log(x)*y) by pow(x,y)
//POTENTIAL_OPTIMIZATION_POINT: Vitter paper points out an exponentially distributed random var can speed this up
//Vitter, J.S. - An Efficient Algorithm for Sequential Random Sampling - ACM Trans. Math. Software 11 (1985), 37-57.
void vitter(I *a,I n,I N) //Method D
{
  I i=0,j=-1, t, qu1= -n+1+N, S, negalphainv=-13, threshold=-negalphainv*n, limit;
  F nreal=n, Nreal=N, ninv=1.0/n, nmin1inv=1.0/(n-1), Vprime=exp(log(rF())*ninv),
    qu1real=-nreal+1.0+Nreal, negSreal, U, X, y1, y2, top, bottom;

  while(n>1 && threshold < N)
  {
    nmin1inv=1.0/(-1.0+nreal);
    while(1)
    {
      while(1)
      {
        X = Nreal * (-Vprime + 1.0);
        S = floor(X);
        if(S<qu1) break;
        Vprime = exp(log(rF())*ninv);
      }
      U = rF(); negSreal=-S;
      y1=exp(log(U*Nreal/qu1real)*nmin1inv);
      Vprime = y1 * (-X/Nreal+1.0)*(qu1real/(negSreal+qu1real));
      if(Vprime <= 1.0) break;
      y2=1.0; top = -1.0+Nreal;
      if(-1+n > S){bottom=-nreal+Nreal;limit=-S+N;}
      else{bottom=-1.0+negSreal+Nreal; limit=qu1;} 
      for(t=N-1;t>=limit;t--)
      {
        y2=(y2*top)/bottom;
        top--; bottom--;
      }
      if(Nreal/(-X+Nreal) >= y1 * exp(log(y2)*nmin1inv)){Vprime=exp(log(rF())*nmin1inv);break;}
      Vprime = exp(log(rF())*ninv);
    }
    j+=S+1;
    a[i++]=j;
    N=-S+(-1+N); Nreal=negSreal+(-1.0+Nreal);
    n--; nreal--; ninv=nmin1inv;
    qu1=-S+qu1; qu1real=negSreal+qu1real;
    threshold+=negalphainv;
  }

  if(n>1) vitter_a(a+i,n,N,j); // if i>0 then n has been decremented
  else
  {
    S = floor(N*Vprime);
    j+=S+1;
    a[i++]=j;
  }
}


