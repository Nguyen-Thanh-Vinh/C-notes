/*input
2
50 2053
3 6
2 3
*/

#include <iostream>
#include <vector>
#include <cstdio>
#include <cmath>
#include <cstring>
#include <string>
#include <cassert>
#include <algorithm>
#include <cstdlib>
#include <numeric>
#include <utility>
#include <tuple>
#include <climits>
#include <fstream>
#include <bitset>
#include <map>
#include <unordered_map>
#include <set>
#include <unordered_set>
#include <stack>
#include <queue>
#include <random>
#include <chrono>
#include <ios>
#include <iomanip>
#include <functional>
#include <array>

using namespace std;

#define FOR(i, a, b) for (int i = a; i <= b; ++i)
#define FORA(i, a) for (auto &i : a)
#define FORB(i, a, b) for (int i = a; i >= b; --i)
#define SZ(a) ((int) a.size())
#define ALL(a) begin(a), end(a)

typedef int64_t ll;
typedef pair<int, int> pii;
typedef pair<ll, ll> pll;
typedef vector<int> vi;
typedef vector<ll> vl;
#define fi first
#define se second

// start of code

struct MaximumMatching
{
	int N;
	vector<int> matchX, matchY, prv, root;
	vector<vector<int>> adj;

	void init(int _N)
	{
		N = _N;
		adj.assign(N, vector<int>(0));
	}

	void addEdge(int x, int y) { adj[x].push_back(y); }

	void solve()
	{
		matchX.assign(N, -1);
		matchY.assign(N, -1);

		// greedy matching
		// FOR(x, 0, N - 1) {
		// 	FORA(y, adj[x]) {
		// 		if (!~matchY[y]) {
		// 			matchX[x] = y, matchY[y] = x;
		// 			break;
		// 		}
		// 	}
		// }

		bool run = true;

		while (run) {
			prv.assign(N, -1);
			root.assign(N, -1);
			run = false;

			queue<int> qu;
			FOR(x, 0, N - 1) if (!~matchX[x]) root[x] = x, qu.push(x);
			while (!qu.empty()) {
				int x = qu.front(); qu.pop();
				if (~matchX[root[x]]) continue;
				for (int y : adj[x]) {
					if (~matchY[y]) {
						int nxt = matchY[y];
						if (!~root[nxt]) {
							prv[y] = x;
							root[nxt] = root[x];
							qu.push(nxt);
						}
					} else {
						run = true;
						for (int i = matchX[x]; ~i; swap(i, matchX[matchY[i] = prv[i]]));
						matchX[x] = y, matchY[y] = x;
						break;
					}
				}
			}
		}
	}

	int get(int x) { return matchX[x]; }
} matching;

void check(vector<vector<int>> A, int N, int K)
{
	assert(SZ(A) == N && SZ(A[0]) == N);

	FOR(i, 0, N - 1) K -= A[i][i];
	assert(K == 0);

	FOR(i, 0, N - 1) {
		vi cnt(N, 0);
		FOR(j, 0, N - 1) cnt[A[i][j] - 1] = 1;
		assert(count(ALL(cnt), 1) == N);
	}

	FOR(i, 0, N - 1) {
		vi cnt(N, 0);
		FOR(j, 0, N - 1) cnt[A[j][i] - 1] = 1;
		assert(count(ALL(cnt), 1) == N);
	}
}

vector<vector<int>> solve(int N, int K)
{
	vector<vector<int>> A(N, vector<int>(N, 0));
	if (K % N == 0) {
		int X = K / N;
		FOR(i, 0, N - 1) FOR(j, 0, N - 1) {
			A[i][j] = (X + i - j + N - 1) % N + 1;
		}
		return A;
	}

	int X = -1, Y, Z;
	FOR(x, 1, N) FOR(y, 1, N) {
		int z = K - x * (N - 2) - y;
		if (1 <= z && z <= N && x != y && x != z) {
			X = x, Y = y, Z = z;
		}
	}
	assert(~X);

	FOR(i, 2, N - 1) A[i][i] = X;
	A[0][0] = Y, A[1][1] = Z, A[0][1] = A[1][0] = X;

	auto fillA = [&](int x) {
		matching.init(N);
		vector<int> usedRow(N, 0), usedCol(N, 0);
		FOR(i, 0, N - 1) FOR(j, 0, N - 1) if (A[i][j] == x) usedRow[i] = usedCol[j] = 1;
		FOR(i, 0, N - 1) FOR(j, 0, N - 1) if (!usedRow[i] && !usedCol[j] && !A[i][j]) {
			matching.addEdge(i, j);
		}
		matching.solve();
		FOR(i, 0, N - 1) if (!usedRow[i]) {
			// cout << i << flush;
			A[i][matching.get(i)] = x;
		}
	};

	fillA(Y);
	// FOR(i, 0, N - 1) FOR(j, 0, N - 1) cout << A[i][j] << " \n"[j == N - 1]; cout << flush;
	if (Y != Z) fillA(Z);

	FOR(i, 1, N) if (i != X && i != Y && i != Z) fillA(i);

	// FOR(i, 0, N - 1) FOR(j, 0, N - 1) cout << A[i][j] << " \n"[j == N - 1]; cout << flush;
	// check(A, N, K);
	return A;
}

int32_t main()
{
	ios_base::sync_with_stdio(0); cin.tie(0); cout.tie(0);
	// solve(5, 9);

	// FOR(N, 2, 50) {
	// 	cout << "try " << N << endl;
	// 	FOR(K, N, N * N) {
	// 		cout << "cur " << K << endl;
	// 		if (K != N + 1 && K != N * N - 1 && (N != 3 || K % N == 0)) solve(N, K);
	// 	}
	// }
	// return 0;

	int T;
	cin >> T;
	FOR(t, 1, T) {
		int N, K;
		cin >> N >> K;
		cout << "Case #" << t << ": ";
		if (K == N + 1 || K == N * N - 1 || (N == 3 && K % N != 0)) cout << "IMPOSSIBLE\n";
		else {
			cout << "POSSIBLE\n";
			auto ans = solve(N, K);
			FOR(i, 0, N - 1) {
				FOR(j, 0, N - 1) {
					cout << ans[i][j] << ' ';
				}
				cout << '\n';
			}
		}
	}

	return 0;
}
