#include<bits/stdc++.h>

using namespace std;

#define fi first 
#define se second
#define FOR(a, b, c) for (int a = b; a <= c; ++a)
#define FORW(a, b, c) for(int a = b; a >= c; --a)
#define pb push_back
#define SZ(a) ((int)a.size())
#define int long long

const int N = 2e2;
const int oo = 1e9;
const int mod = 1e9 + 7;

typedef pair<int, int> ii;



signed main(){  
	freopen("alchemy_input.txt", "r", stdin);
	freopen("alchemy.out", "w", stdout);
    ios_base::sync_with_stdio(false);
    cin.tie(0); 
    cout.tie(0);
    int t; cin >> t;
    FOR(cases, 1, t)	{
    	int n; cin >> n;
    	int cnta = 0, cntb = 0;
    	FOR(i, 1, n) {
    		char c; cin >> c;
    		if(c == 'A') cnta += 1;
    		else cntb += 1;
    	}

    	char res = (abs(cnta - cntb) == 1) ? 'Y' : 'N';
    	cout << "Case #" << cases << ": " << res << '\n';
    }
}	