#include <iostream>
#include <time.h>
#include <math.h>
#include <vector>
#define INF 30000

using namespace std;

void get_matrices(int n, int v, int c, int **m_a, double **m_d)
{
	int i, j, need = 0;
	double *x, *y;
	x = new double[n];
	y = new double[n];
	for (i = 0; i < n; i++)
	{
		cin >> m_a[v][i];
		need += m_a[v][i];
		cin >> x[i];
		cin >> y[i];
	}
	m_a[v][0] = v * c - need;
	cout << "----------------" << endl;
	for (i = 0; i < v; i++)
		m_a[i][n] = c;
	m_a[v][n] = 0;
	for (i = 0; i < v; i++)
		for (j = 0; j < n; j++)
			m_a[i][j] = 0;
	cout << "----------------" << endl;
	for (i = 0; i < n; i++)
	{
		m_d[i][i] = INF;
		for (j = i + 1; j < n; j++)
		{
			m_d[i][j] = sqrt((x[i] - x[j]) * (x[i] - x[j]) + (y[i] - y[j]) * (y[i] - y[j]));
			m_d[j][i] = m_d[i][j];
		}
	}
}

//ВЫСЧИТЫВАТЬ КАЖДЫЙ РАЗ НОВУЮ МАТРИЦУ РАССТОЯНИЙ И ПЕРЕДАВАТЬ В SOLVING

void get_reference(int n, int v, int **m)
{
	int i, j, mn, min_j = 0;
	for (i = 0; i < v; i++)
	{
		for (j = min_j; j < n; j++)
		{
			if (m[i][n] == 0)
			{
				break;
			}
			if (m[v][j] == 0)
			{
				min_j = j + 1;
				continue;
			}
			mn = min(m[i][n], m[v][j]);
			m[i][j] = mn;
			m[i][n] -= mn;
			m[v][j] -= mn;
		}
	}
}

double *gauss(double **a, double *y, int n, double *x) 
//prog-cpp.ru/gauss/
{
  double max;
  int k, index;
  const double eps = 0.0000001;  // точность
  k = 0;
  while (k < n) 
  {
    // Поиск строки с максимальным a[i][k]
    max = abs(a[k][k]);
    index = k;
    for (int i = k + 1; i < n; i++) 
    {
      if (abs(a[i][k]) > max)
      {
        max = abs(a[i][k]);
        index = i;
      }
    }
    // Перестановка строк
    if (max < eps) 
    {
//	    return x;
      // нет ненулевых диагональных элементов
 //     cout << "Решение получить невозможно из-за нулевого столбца ";
//      cout << index << " матрицы A" << endl;
  //    return 0;
    }
    for (int j = 0; j < n; j++) 
    {
      double temp = a[k][j];
      a[k][j] = a[index][j];
      a[index][j] = temp;
    }
    double temp = y[k];
    y[k] = y[index];
    y[index] = temp;
    // Нормализация уравнений
    for (int i = k; i < n; i++) 
    {
      double temp = a[i][k];
      if (abs(temp) < eps) continue; // для нулевого коэффициента пропустить
      for (int j = 0; j < n; j++) 
        a[i][j] = a[i][j] / temp;
      y[i] = y[i] / temp;
      if (i == k)  continue; // уравнение не вычитать само из себя
      for (int j = 0; j < n; j++)
        a[i][j] = a[i][j] - a[k][j];
      y[i] = y[i] - y[k];
    }
    k++;
  }
  // обратная подстановка
  for (k = n - 1; k >= 0; k--)
  {
    x[k] = y[k];
    for (int i = 0; i < k; i++)
      y[i] = y[i] - a[i][k] * x[k];
  }
  return x;
}

void solve_system(int n, int v, int **m_a, double **m_d, double *solving, double **m_n)
{
	double **m_p, *y;
	int i, j, k, counter = 0, nv = n + v;
	bool *anyi, *anyj, any = 0;
	vector <int> chngi, chngj;
	y = new double[n + v];
	m_p = new double*[n + v];
	for (i = 0; i < n + v; i++)
	{
		m_p[i] = new double[n + v];
		for (j = 0; j < n + v; j++)
			m_p[i][j] = 0;
	}
	for (i = 0; i < v; i++)
	{
		for (j = 0; j < n; j++)
		{
			if (m_a[i][j] != 0)
			{
				m_p[counter][i] = 1;
				m_p[counter][v + j] = 1;
				y[counter] = m_n[i][j];
				counter++;
			}
		}
	}
	nv = counter - 1;
	anyi = new bool[n + v];
	anyj = new bool[n + v];
	for (i = 0; i < n + v; i++)
	{
		for (j = 0; j < n; j++)
			if (m_p[i][j] == 1)
			{
				any = 1;
				break;
			}
		if (any == 1)
			anyi[i] = 1;
		else
		{
			anyi[i] = 0;
			chngi.push_back(i);
			chngi.push_back(0);
		}
	}
	if (n + v - chngi.size() / 2 < nv)
		nv = n + v - chngi.size();
	any = 0;
	for (j = 0; j < n + v; j++)
	{
		for (i = 0; i < n; i++)
			if (m_p[i][j] == 1)
			{
				any = 1;
				break;
			}
		if (any == 1)
			anyj[j] = 1;
		else
		{
			anyj[j] = 0;
			chngj.push_back(j);
			chngj.push_back(0);
		}
	}
	if (n + v - chngj.size() / 2 < nv)
		nv = n + v - chngj.size() / 2;
	k = n + v;
	for (i = 0; i < chngi.size(); i += 2)
	{
		while (anyi[k] == 0)
			k--;
		chngi[i + 1] = k;
		for (j = 0; j < n + v; j++)
			if (m_p[k][j] == 1)
				m_p[chngi[i]][j] = 1;
		k--;
	}
	k = n + v;
	for (j = 0; j < chngi.size(); j += 2)
	{
		while (anyi[k] == 0)
			k--;
		chngi[j + 1] = k;
		for (i = 0; i < n + v; i++)
			if (m_p[i][k] == 1)
				m_p[chngi[i]][j] = 1;
		k--;
	}
	cout << nv << chngi.size() << chngj.size() << "----------------" << endl;
//	nv--;
	for (i = 0; i < nv; i++)
	{
		for (j = 0; j < n + v; j++)
			cout << m_p[i][j] << " ";
		cout << endl;
	}
	cout << "----------------" << endl;
	for (i = 0; i < nv; i++)
		cout << y[i] << " ";
	cout << endl;
	cout << "----------------" << endl;
	solving = gauss(m_p, y, nv, solving);
	for (i = 0; i < n + v; i++)
		cout << solving[i] << endl;
}

bool negative(int n, int v, double **m, int **m_a, int *imn, int *jmn)
{
	double mn = INF;
	int i, j;
	for (i = 0; i < v; i++)
	{
		for (j = 0; j < n; j++)
		{
			if (m[i][j] < 0 && m[i][j] < mn && m_a[i][j] == 0)
			{
				mn = m[i][j];
				*imn = i;
				*jmn = j;
			}
		}
	}
	if (mn == INF)
		return false;
	else
		return true;
}

void get_matrix_of_costs(int n, int v, int **m_a, double **m_d, double **m_n)
{
	int i, j, k, mnk, imn, jmn;
	vector <int> vert;
//	if (n > 5)
	//	eps = 0.5;
	for (i = 0; i < v; i++)
	{
		for (j = 0; j < n; j++)
			if (m_a[i][j] != 0)
				vert.push_back(j);
		while (vert.size() > 1)
		{
			mnk = 1;
			for (k = 1; k < vert.size(); k++)
			{
				if (m_d[vert[0]][vert[k]] < m_d[vert[0]][vert[mnk]])
					mnk = k;
			}
//			cout << i << "!!!" << vert[0] << "!!!" << vert[mnk] << "!!!" << endl;
			m_n[i][vert[0]] = m_d[i][vert[mnk]];
			vert[0] = vert[mnk];
			vert[mnk] = vert[vert.size() - 1];
			vert.pop_back();
		}
		for (j = 0; j < n; j++)
			if (m_n[i][j] != 0)
			{
				m_n[i][j] = m_d[vert[0]][j];
			}
		vert.clear();
	}
//	cout << "+++++++++" << endl;
	for (i = 0; i < v; i++)
	{
//		for (j = 0; j < n; j++)
//			cout << m_n[i][j] << " ";
//		cout << endl;
	}
}

vector <int> find_cycle(int n, int v, int **m_a, int *imn, int *jmn)
{
	vector <int> been;
	bool flag = 0, end = 0, **new_m, any = 0;
	int i, j;
	new_m = new bool*[v];
	for (i = 0; i < v; i++)
	{
		new_m[i] = new bool[n];
		for (j = 0; j < n; j++)
			if (m_a[i][j] == 0)
				new_m[i][j] = 0;
			else
				new_m[i][j] = 1;
	}
	new_m[*imn][*jmn] = 0;
	for (i = 0; i < v; i++)
		if (new_m[i][*jmn] == 1)
		{
			any = 1;
			new_m[i][*jmn] = 0;
			been.push_back(i);
			break;
		}
	if (any == 0)
		return been;
	cout << "---" << endl;
	while (end == 0)
	{
		any = 0;
		cout << been[been.size() - 1] << endl;
		if (flag == 0)
		{
			//horizontal search
			for (j = 0; j < n; j++)
			{
				if (new_m[been[been.size() - 1]][j] == 1)
				{
					any = 1;
					new_m[been[been.size() - 1]][j] = 0;
					been.push_back(j);
					break;
				}
			}
			if (any == 0)
				been.pop_back();
			flag = 1;
		}
		else
		{
			//vertical search
			if (new_m[*imn][been[been.size() - 1]] == 1)
			{
				return been;
			}
			for (i = 0; i < v; i++)
				if (new_m[i][been[been.size() - 1]] == 1)
				{
					any = 1;
					new_m[i][been[been.size() - 1]] = 0;
					been.push_back(i);
					break;
				}
			if (any == 0)
				been.pop_back();
			flag = 0;
		}
		if (been.size() == 0)
			end = 1;
	}
	return been;
}

int change_m_amount(int n, int v, int **m_a, double **m_n, int *imn, int *jmn)
{
	int i, mn;
	vector <int> cycle;
//	cout << "----------------" << endl;
	cycle = find_cycle(n, v, m_a, imn, jmn);
	if (cycle.size() == 0)
		return -1;
//	cout << "----------------" << endl;
//	for (i = 0; i < cycle.size(); i++)
//		cout << cycle[i] << "|";
//	cout << endl;
	mn = m_a[cycle[0]][*jmn];
	for (i = 2; i < cycle.size(); i += 2)
		mn = min(mn, m_a[cycle[i]][cycle[i - 1]]);
	mn = min(mn, m_a[*imn][cycle[i - 1]]);
	m_a[cycle[0]][*jmn] -= mn;
	for (i = 2; i < cycle.size(); i += 2)
		m_a[cycle[i]][cycle[i - 1]] -= mn;
	m_a[*imn][cycle[i - 1]] -= mn;
	m_a[*imn][*jmn] += mn;
	for (i = 1; i < cycle.size(); i += 2)
		m_a[cycle[i - 1]][cycle[i]] += mn;
	cout << mn << endl;
	return 0;
}

/*void change_m_amount(int n, int v, int **m_a, double **m_n, int *imn, int *jmn)
{
	int i, j, mn;
	for (j = 0; j < n; j++)
	{
		if (m_a[*imn][j] != 0)
		{
			for (i = 0; i < v; i++)
			{
				if (i != *imn && m_a[i][j] * m_a[i][*jmn] != 0)
				{
					mn = min(m_a[*imn][j], m_a[i][*jmn]);
					m_a[i][j] += mn;
					m_a[*imn][*jmn] += mn;
					m_a[*imn][j] -= mn;
					m_a[i][*jmn] -= mn;
					return;
				}
			}
		}
	}
}*/

void change_m_n(int n, int v, int **m_a, double **m_d, double **m_n, int imn, int jmn)
{
}

double calculate_len(int n, int v, int **m_a, double **m_n)
{
	double len = 0;
	int i, j;
	for (i = 0; i < v; i++)
	{
		for (j = 0; j < n; j++)
		{
			if (m_a[i][j] > 0)
				len += m_n[i][j];
			else
				len -= m_n[i][j];
		}
	}
	len += m_n[v - 1][n - 2];
	while (len > INF)
	{
		len -= INF;
	}
	if (n > 5)
		len *= 0.5;
//	for (i = 0; i < v; i++)
//	{
//		for (j = 0; j < n; j++)
//			cout << m_n[i][j] << "+++";
//		cout << endl;
//	}
	return len;
}

double potential_method(int n, int v, int **m_a, double **m_d)
{
	double *solving, **m_p, **m_n, len = 0;
	vector <double> l;
	int i, j, imn, jmn;
	m_p = new double*[v];
	m_n = new double*[v];
	for (i = 0; i < v; i++)
	{
		m_p[i] = new double[n];
		m_n[i] = new double[n];
		for (j = 0; j < n; j++)
		{
			m_p[i][j] = 0;
			m_n[i][j] = 0;
		}
	}
	get_matrix_of_costs(n, v, m_a, m_d, m_n);
	solving = new double[n + v];
	solve_system(n, v, m_a, m_d, solving, m_n);
	for (i = 0; i < v; i++)
	{
		for (j = 0; j < n; j++)
		{
			m_p[i][j] = solving[i] + solving[v + j];
		}
	}
//	cout << "----------------" << endl;
//	cout << "----------------" << endl;
	for (i = 0; i < v; i++)
	{
		for (j = 0; j < n; j++)
		{
			m_p[i][j] -= m_n[i][j];
//			cout << m_p[i][j] << " ";
		}
//		cout << endl;
	}
	while (negative(n, v, m_p, m_a, &imn, &jmn))
	{
		if (change_m_amount(n, v, m_a, m_n, &imn, &jmn) == -1)
			break;
		cout << "*************" << imn << "********" << jmn << endl;
		for (i = 0; i < v; i++)
		{
			for (j = 0; j < n; j++)
				cout << m_a[i][j] << " ";
			cout << endl;
		}
//		change_m_n(n, v, m_a, m_d, m_n, imn, jmn);
		get_matrix_of_costs(n, v, m_a, m_d, m_n);
		delete(solving);
		solving = new double[n + v];
		solve_system(n, v, m_a, m_d, solving, m_n);
		for (i = 0; i < v; i++)
		{
			for (j = 0; j < n; j++)
			{
				m_p[i][j] = solving[i] + solving[v + j];
			}
		}
//		return calculate_len(n, v, m_a, m_n);
		len = calculate_len(n, v, m_a, m_n);
		l.push_back(len);
	}
	for (i = 0; i < l.size(); i++)
		cout << l[i] << "'";
	cout << endl;
	return len;
}

int main()
{
	int v, c, n, i, j, **m_amount;
	double **m_distance, len;
	cout << time(NULL) << endl;
	cin >> n;
	cin >> v;
	cin >> c;
	cout << n << v << c << endl;
	m_distance = new double*[n];
	for (i = 0; i < n; i++)
		m_distance[i] = new double[n];
	cout << time(NULL) << endl;
	m_amount = new int*[v + 1];
	for (i = 0; i < v + 1; i++)
		m_amount[i] = new int[n + 1];
	cout << time(NULL) << endl;
	get_matrices(n, v, c, m_amount, m_distance);
	cout << "----------------" << endl;
	for (i = 0; i < v; i++)
	{
		for (j = 0; j < n; j++)
			cout << m_distance[i][j] << " ()";
		cout << endl;
	}
	cout << "----------------" << endl;
	for (i = 0; i < v + 1; i++)
	{
		for (j = 0; j < n + 1; j++)
			cout << m_amount[i][j] << " ";
		cout << endl;
	}
	get_reference(n, v, m_amount);
	cout << "----------------" << endl;
	len = potential_method(n, v, m_amount, m_distance);
	for (i = 0; i < v + 1; i++)
	{
		for (j = 0; j < n + 1; j++)
			cout << m_distance[i][j] << " ";
		cout << endl;
	}
	cout << len << "----------------" << endl;
	cout << time(NULL) << endl;
	return 0;
}
