#include <stdio.h>
#include <stdlib.h> 
#include <iostream>
#include <math.h>
#include <vector>
#include <malloc.h>
#include <time.h>
#include <unistd.h>
#define INF 30000

using namespace std;

struct edge
{

	int first;
	int second;
	double cost;
};

struct edge *input_data(struct edge *e, int n, /*double *x1, double *y1,*/  vector <int> *need)
{
	int i, j, cur = 0;
	double *x, *y;
//	cout << n << "(((((((((" << endl;
	x = new double[n];
	y = new double[n];
//	cout << "(((((((((" << endl;
	for (i = 0; i < n; i++)
	{
		cin >> j;
		need[0].push_back(j);
		cin >> x[i];
		cin >> y[i];
	}
//	cout << "(********(" << endl;
	for (i = 0; i < n; i++)
		for (j = 0; j < n; j++)
		{
			if (i != j)
			{
				e[cur].first = i;
				e[cur].second = j;
				e[cur].cost = sqrt((x[i] - x[j]) * (x[i] - x[j]) + (y[i] - y[j]) * (y[i] - y[j]));
				cur++;
			}
		}
	return e;
}

int ford_bellman(int cur, int n, struct edge *e, double *length, vector <int> been, double *d)
// return [mn, nmn], where nmn - number of nearest vertice, mn - min path lenght
{
	int i, nmn, any = 1;
	double mn;
	d[0] = 0;
	for (i = 1; i < n; i++)
		d[i] = INF;
//	cout << "+++++++++++++++" << n << endl;
	while (any)
	{
        	any = 0;
        	for (i = 0; i < (n * (n - 1)); i++)
        		if (d[e[i].first] < INF)
       				if (d[e[i].second] > d[e[i].first] + e[i].cost)
       				{
        				any = 1;
                			d[e[i].second] = d[e[i].first] + e[i].cost;
				}
 	}
//	cout << "+++++++++++++++" << n << endl;
//	for (i = 0; i < n; i++)
//		cout << d[i] << "	";
//	cout << endl;
	for (i = 0; i < been.size(); i++)
	{
		d[been[i]] = INF;
	}
//	cout << "+++++++++++++++" << n << endl;
//	for (i = 0; i < n; i++)
//		cout << "^^^^^^" << d[i];
//	cout << endl;
	mn = INF;
	d[cur] = INF;
	for (i = 1; i < n; i++)
	{
		if (d[i] < mn)
		{
			mn = d[i];
			nmn = i;
		}
	}
	if (mn == INF)
		return -1;
	*length = *length + mn;
//	cout << "_______" << nmn << endl;
//	cout << mn << ":::::::" << nmn << endl;
	return nmn;
}

struct edge *change(int n, struct edge *e, int cur, double *d0)
{
	int i;
	double c;
	if (cur <= 0 || cur >= n)
		return e;
	for (i = 0; i < n * (n - 1); i++)
	{
		if (e[i].first == 0)
			e[i].first = cur;
		else if (e[i].first == cur)
			e[i].first = 0;
		if (e[i].second == 0)
			e[i].second = cur;
		else if (e[i].second == cur)
			e[i].second = 0;
	}
	c = d0[0];
	d0[0] = d0[cur];
	d0[cur] = c;
	return e;
}

double minimum(double *d, int n)
{
	int i;
	double mn;
	mn = d[0];
	for (i = 1; i < n; i++)
		if (d[i] < mn)
			mn = d[i];
	return mn;
}

double n_maximum(double *d, int n)
{
	int i, nmx;
	double mx;
	mx = 0;
	for (i = 0; i < n; i++)
		if (d[i] > mx && d[i] != INF)
		{
			mx = d[i];
			nmx = i;
		}
	return nmx;
}

vector <int> sort_by_need(struct edge *e, vector <int> need)
{
	int i, j, mx, nmx;
	struct edge c;
	for (i = 1; i < need.size(); i++)
	{
		nmx = i;
		mx = need[nmx];
		for (j = i + 1; j < need.size(); j++)
		{
			if (need[j] > mx)
			{
				nmx = j;
				mx = need[nmx];
			}
		}
		need[nmx] = need[i];
		need[i] = mx;
		c = e[i];
		e[i] = e[nmx];
		e[nmx] = c;
	}
	return need;
}

vector <vector <int>> distrib(vector <vector <int>> vehs, vector <int> need, int vehicles, int capacity)
{
	int i, j, cur_cap, c, sum = 0;
	vector <int> veh, added;
	for (i = 0; i < need.size(); i++)
		added.push_back(0);
//	for (i = 0; i < need.size(); i++)
//		cout << need[i] << " ";
	cout << vehicles << "*****" << need.size() << endl;
	j = 1;
	for (i = 0; i < vehicles; i++)
	{
		cur_cap = 0;
		while (j < need.size() && cur_cap < capacity)
		{
			if (need[j] + cur_cap <= capacity && added[j] == 0)
			{
				cur_cap += need[j];
				veh.push_back(j);
				added[j] = 1;
				sum++;
//				if (j == need.size() - 1)
//					j = INF;
			}
			j++;
		}
		if (j >= need.size())
			j = 1;
		else
			j--;
		vehs.push_back(veh);
		veh.clear();
		if (sum == need.size() - 1)
			break;
	}
	cout << i << " " << vehicles << endl;
	j = 0;
	while (i < vehicles)
	{
		if (vehs[j].size() > 1)
		{
			veh.push_back(vehs[j][vehs[j].size()]);
			vehs.push_back(veh);
			vehs[j].pop_back();
			i++;
		} else
			j = -1;
		j++;
		if (i == need.size() - 2)
			break;
	}
//	cout << "*****" << capacity << endl;
	for (i = 0; i < vehs.size(); i++)
	{
		for (j = 0; j < vehs[i].size(); j++)
			cout << vehs[i][j] << " ";
		cout << endl;
	}
	return vehs;
}

double find_way(int n, struct edge *e, vector <int> veh)
{
	int cur = 0, i;
	double *d, *d0, length = 0;
	vector <int> been;
	d0 = new double[n];
	d = new double[n];
	cur = ford_bellman(cur, n, e, &length, been, d0);
	for (i = 0; i < n - 1; i++)
	{
		been.push_back(cur);
		cur = ford_bellman(cur, n, e, &length, been, d);
		e = change(n, e, cur, d);
	}
	length += d0[cur];
	return length;
}

vector <vector <int>> rand_changes(vector <vector <int>> vehs, vector <int> need)
{
	int i, r, j, c, a, b, flag, x;
	if (vehs.size() == 0)
		return vehs;
	r = rand() % (2 * vehs.size());
	cout << "!!!" << r << endl;
	for (i = 0; i < r; i++)
	{
		a = rand() % vehs.size();
		b = rand() % vehs.size();
		cout << endl << a << "&&&&" << b << endl;
		if (a == b || vehs[a].size() == 0)
			continue;
		x = rand() % vehs[a].size();
		cout << x << "@@@" << endl;
		flag = 1;
		while (flag == 1 && x < vehs[a].size())
		{
			for (j = 0; j < vehs[b].size(); j++)
			{
				if (need[vehs[b][j]] == need[vehs[a][x]])
				{
					flag = 0;
					c = vehs[b][j];
					vehs[b][j] = vehs[a][x];
					vehs[a][x] = c;
					cout << "Z" << endl;
					break;
				}
			}
			x++;
		}
	}
	cout << "		" << endl;
	for (i = 0; i < vehs.size(); i++)
	{
		for (j = 0; j < vehs[i].size(); j++)
			cout << vehs[i][j] << " ";
		cout << endl;
	}
	return vehs;
}
/*vector <vector <int>> change_vehs(vector <vector <int>>, vector <int> need)
{
	int i, j;
	for (i = 0; i < vehs.size(); i++)
	{
		while 
	}
}*/

double solving(int n, struct edge *e, vector <int> need, int vehicles, int capacity)
{
	int length = 0, i, j, cnst = 50;
	double prevlen = 0;
	struct edge new_e;
	vector <vector <int>> vehs, prevvehs;
	need = sort_by_need(e, need);
	vehs = distrib(vehs, need, vehicles, capacity);
	for (i = 0; i < vehs.size(); i++)
		prevlen += find_way(n, e, vehs[i]);
	prevvehs = vehs;
	for (j = 0; j < cnst; j++)
	{
		vehs = rand_changes(vehs, need);
		for (i = 0; i < vehs.size(); i++)
			length += find_way(n, e, vehs[i]);
		if (prevlen > length)
		{
			prevlen = length;
			prevvehs = vehs;
		}

	}
	return prevlen;
}

int main()
{
	struct edge *e;
	int n, vehicles, capacity;
	double length;
	vector <int> need;
//	randomize();
	srand((time(NULL) + getpid() % 100));
//	cout << rand() << " " << rand() << endl;
//	cout << time(NULL) << " " << getpid() << endl;
	cin >> n;
	cin >> vehicles;
	cin >> capacity;
	e = new edge[n * (n - 1)];
	e = input_data(e, n, &need);
	length = solving(n, e, need, vehicles, capacity);
	cout << length << endl;
}
	
	
/*int main()
{
	int cur_back = 0, cnst = 150, remember, first_cur = 0, i, j, n, all, cur = 0, cur_cap, capacity, vehicles, cur_veh = 0;
	double x, y, x0, y0;
	vector <int> been;
	vector <int> need, way;
	vector <vector <int>> ways;
	double length = 0, *d0, *d;
	struct edge *e;
	cin >> all;
	cin >> vehicles;
	cin >> capacity;
	d0 = new double[all];
	d = new double[all];
	e = new edge[all * (all - 1)];
	e = input_data(e, all, &x, &y, &need);
	cur = ford_bellman(cur, all, e, &length, been, d0, capacity, need);
	been.push_back(0);
	while (been.size() < all)
	{
		if (cur_veh >= vehicles)
		{
			cout << "Error";
//			return 0;
			break;
		}
		cur_veh++;
		cur_cap = 0;
//		way.push_back(0);
		cur_back = cur;
		cur_cap += need[cur];
//		for (i = 0; i < all * (all - 1); i++)
//			cout << e[i].cost << " " << e[i].first << " " << e[i].second << endl;
//		cout << "*****" << capacity << endl;
		while (cur_cap < capacity)
		{
			way.push_back(cur);
//			cout << "+++++++++++++++" << all << endl;
			been.push_back(cur);
//			cout << "&&&&&&&&" << cur << endl;
			remember = d0[cur_back];
			e = change(all, e, cur, d);
//			for (i = 0; i < all * (all - 1); i++)
//				cout << e[i].cost << "|" << e[i].first << "|" << e[i].second << endl;
//			cout << "*****" << endl;
//			for (i = 0; i < been.size(); i++)
//				cout << been[i];
			cur = ford_bellman(cur, all, e, &length, been, d, capacity - cur_cap, need);
			if (cur == -1)
				break;
 //			cout << "))))" << length << endl;
			cur_cap += need[cur];
		}
//		cout << cur_back << endl;
		e = change(all, e, cur_back, d);
		cur_back = cur;
		length += d0[first_cur];
//		cout << length << endl;
		for (i = 0; i < been.size(); i++)
			d0[been[i]] = INF;
		first_cur = cur;
//		for (i = 0; i < way.size(); i++)
//			cout << way[i] << endl;
		if (way.size() == 1)
			length += -INF + remember;
		ways.push_back(way);
		way.clear();
		cur = n_maximum(d0, all);
	}
	for (i = 0; i < ways.size(); i++)
	{	
		for (j = 0; j < ways[i].size(); j++)
			cout << ways[i][j] << " ";
		cout << endl;
	}
//	for (i = 0; i < all; i++)
//		cout << d0[i];
	cout << endl;
	cout << length << endl;
	return 0;
*/
