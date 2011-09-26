int idamax_(int const*, double const*, int const*);
void dgemv_(char*, int const*, int const*, double const*, double const*, int const*, double const*, int const*, double const*, double*, int const*, int);
void dcopy_(int const*, double const*, int const*, double*, int const*);
void daxpy_(int const*, double const*, double const*, int const*, double*, int const*);
void dscal_(int const*, double const*, double*, int const*);
void dtrsv_(char*, char*, char*, int const*, double const*, int const*, double*, int const*, int, int, int);
void dger_(int const*, int const*, double const*, double const*, int const*, double const*, int const*, double*, int const*);
void dgemm_(char*, char*, int const*, int const*, int const*, double const*, double const*, int const*, double const*, int const*, double const*, double*, int const*, int, int);
void dtrsm_(char*, char*, char*, char*, int const*, int const*, double const*, double const*, int const*, double*, int const*, int, int, int, int);
