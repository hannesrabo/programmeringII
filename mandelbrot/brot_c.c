#include <stdio.h>
#include <erl_nif.h>

// This is compiled as folows:
// gcc -o brot_c.so -fpic -shared brot_c.c -I /usr/lib/erlang/erts-8.2/include

extern int test_c(int x);

static ERL_NIF_TERM test_c_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  double Cr, Ci, P, Q;
  long double Zr, Zi, Zsr, Zsi, tmp;
  int i, m;

  /*
3 7 -1.13 -0.6900000000000002
4 12 -0.9199999999999999 -0.6900000000000002
5 0 -0.71 -0.6900000000000002
10 0 -0.5 -0.6900000000000002

   */
  
  //Creating all variables
  enif_get_double(env, argv[0],&Cr);
  enif_get_double(env, argv[1],&Ci);
  enif_get_int(env, argv[2], &m);

  Zr = 0;
  Zi = 0;
  Zsr = 0;
  Zsi = 0;
  i = 0;

  // Checking if in range
  P = Cr - 0.25;
  Q = P * P + Ci * Ci;

  if(Q*(Q + P) > Ci * Ci * 0.25) {
    
    // Calculate the mandelbrot value
    while( i < m) {
      // End the loop: The variable escaped
      if((Zsr + Zsi) >= 4) {
	return enif_make_tuple3(env, enif_make_int(env, i), enif_make_double(env, Zr), enif_make_double(env, Zi));
      }

      // Do the real calculations
      tmp = Zr * Zi;
      Zr = Zsr - Zsi + Cr;
      Zi = tmp + tmp + Ci;
      
      //Update
      Zsr = Zr * Zr;
      Zsi = Zi * Zi;
      i++;

    }

    // Got stuck in here (i > m before return)
    return enif_make_tuple3(
			    env, enif_make_int(env, 0),
			    enif_make_double(env, Zr),
			    enif_make_double(env, Zi)
			    );
    
  } else {
    // Return 0 and input args if out of range
    return enif_make_tuple3(env, enif_make_int(env, i), enif_make_double(env, Cr), enif_make_double(env, Ci));
    
  }
}


static ErlNifFunc nif_funcs[] = {
  {"test_c", 3, test_c_nif}
};

ERL_NIF_INIT(brot, nif_funcs, NULL, NULL, NULL, NULL)
