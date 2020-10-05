#include<bits/stdc++.h>

using namespace std;

#define fi first 
#define se second
#define FOR(a, b, c) for (int a = b; a <= c; ++a)
#define FORW(a, b, c) for(int a = b; a >= c; --a)
#define pb push_back
#define SZ(a) ((int)a.size())
#define int long long

const int N = 4e6;
const int oo = 1e9;
const int mod = 1e9 + 7;

typedef pair<int, int> ii;

int n;
int f[N], g[N], pos[N], root[N];
vector<int> tmp;
ii val[N];
map<int, int> mp;
vector<ii> in[N];

signed main(){  
	freopen("timber_input.txt", "r", stdin);
	freopen("timber.out", "w", stdout);
    ios_base::sync_with_stdio(false);
    cin.tie(0); 
    cout.tie(0);
    int t; cin >> t;
    FOR(cases, 1, t)	{
        cin >> n;
        int ans = 0;
        tmp.clear();
        mp.clear();

        FOR(i, 1, n) {
            cin >> val[i].fi >> val[i].se;
            tmp.pb(val[i].fi);
            tmp.pb(val[i].fi + val[i].se);
            tmp.pb(val[i].fi - val[i].se);
        }
        sort(tmp.begin(), tmp.end());

        int cnt = 0;
        FOR(i, 0, SZ(tmp) - 1) if(i == SZ(tmp) - 1 || tmp[i] != tmp[i + 1])
            mp[tmp[i]] = ++cnt, pos[cnt] = tmp[i];

        FOR(i, 0, cnt + 1) f[i] = g[i] = 0, root[i] = -1, in[i].clear();

        FOR(i, 1, n) {
            root[ mp[val[i].fi] ] = val[i].se;
            in[ mp[ val[i].fi + val[i].se] ].pb({mp[val[i].fi], val[i].se});
        }

        FOR(i, 1, cnt) {
            for(auto x: in[i]) f[i] = max(f[i], f[x.fi] + x.se);
            g[i] = f[i];
            if(root[i] != -1) {
                int last = mp[ pos[i] - root[i]];
                g[i] = max(g[i], g[last] + root[i]);
            }
            ans = max(ans, g[i]);
        }
        cout << "Case #" << cases << ": " << ans << '\n';
    }   
}	
