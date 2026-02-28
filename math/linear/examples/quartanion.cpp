#include "../src/quartanion.h"
#include <iostream>

int main()
{
    quartanion<double> q(1, 2, 3, 4);
    quartanion<double> r(5, 6, 7, 8);

    std::cout << "------ Conjugate Rules --------" << std::endl;
    auto q_conj = q.conjugate();
    auto q_conj_conj = q_conj.conjugate();
    std::cout << "q:           " << q << std::endl;
    std::cout << "q_conj:      " << q_conj << std::endl;
    std::cout << "q_conj_conj: " << q_conj_conj << std::endl;
    std::cout << "q_conj_conj == q: " << (q_conj_conj == q ? "true" : "false") << std::endl;

    std::cout << std::endl;
    auto q_plus_r = q + r;
    auto q_plus_r_conj = q_plus_r.conjugate();
    auto q_conj_plus_r_conj = q_conj + r.conjugate();
    std::cout << "q* + r*  = " << q_conj_plus_r_conj << std::endl;
    std::cout << "(q + r)* = " << q_plus_r_conj << std::endl;
    std::cout << "q* + r* == (q + r)*: " << (q_conj_plus_r_conj == q_plus_r_conj ? "true" : "false") << std::endl;

    std::cout << std::endl;
    auto q_times_r = q * r;
    auto q_times_r_conj = q_times_r.conjugate();
    auto r_conj_times_q_conj = r.conjugate() * q.conjugate();
    std::cout << "r* * q*  = " << r_conj_times_q_conj << std::endl;
    std::cout << "(q * r)* = " << q_times_r_conj << std::endl;
    std::cout << "r* * q* == (q * r)*: " << (r_conj_times_q_conj == q_times_r_conj ? "true" : "false") << std::endl;
}