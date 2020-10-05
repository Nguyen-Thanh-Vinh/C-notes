#include<bits/stdc++.h>
using namespace std;

#define fi first
#define se second
#define pb push_back
#define FOR(a, b, c) for(int a = b; a <= c; ++a)
#define int long long

const int N = 1e4 + 10;
const int oo = 1e9;
const int mod = 1e9 + 7;
typedef pair<int, int> ii;

const string NAME = "Spm";

int RAND(int l, int r)  {
    return l + (rand() + rand() + rand()) % (r - l + 1);
}

signed main(){
    //freopen("braces.inp", "r", stdin);
    //freopen("braces.out", "w", stdout);
    FOR(test, 1, 100) {
        fstream out("test.inp", fstream::out);

        int n = 15; out << n << '\n';
        FOR(i, 1, n - 1) {
            int v = RAND(i + 1, n);
            out << i << ' ' << v << '\n';
        }

        out.close();
        // check
        system("spm.exe"); system("Spm_trau.exe");
        if(system("fc spm.out Spm_trau.out") != 0) {
            cout << "WA"; return 0;
        }
    }
    cout << "bla";
}
