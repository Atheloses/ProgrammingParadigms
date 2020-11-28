#include <iostream>
#include <omp.h>
#include <windows.h>
#include <iostream>
#include <iomanip>
#include <string>
#include <thread>  
LARGE_INTEGER frequency;        // ticks per second
LARGE_INTEGER startTime, endTime;           // ticks

using namespace std;

void multiply(int* first, int* second, int* result, int size, int var, int threads, int runs) {
	double elapsedTime = 0;

	for (int run = 1; run <= runs; run++) {
		for (int position = 0; position < size * size; position++)
			result[position] = 0;

		QueryPerformanceCounter(&startTime);

#pragma omp parallel for
		for (int i = 0; i < threads; i++)
		{
			int threadRowStart = floor(1.0 * size / threads * omp_get_thread_num());
			int threadRowEnd = floor(1.0 * size / threads * (omp_get_thread_num() + 1)) - 1;
			if (omp_get_thread_num() + 1 == threads)
				threadRowEnd = size;

			switch (var) {
			case 1:
				for (int row = threadRowStart; row < threadRowEnd; row++)
					for (int col = 0; col < size; col++)
						for (int k = 0; k < size; k++)
							result[row * size + col] += first[row * size + k] * second[k * size + col];
				break;
			case 2:
				for (int k = 0; k < size; k++)
					for (int row = threadRowStart; row < threadRowEnd; row++)
						for (int col = 0; col < size; col++)
							result[row * size + col] += first[row * size + k] * second[k * size + col];
				break;
			case 3:
				for (int k = 0; k < size; k++)
					for (int col = 0; col < size; col++)
						for (int row = threadRowStart; row < threadRowEnd; row++)
							result[row * size + col] += first[row * size + k] * second[k * size + col];
				break;
			case 4:
				for (int row = threadRowStart; row < threadRowEnd; row++)
					for (int k = 0; k < size; k++)
						for (int col = 0; col < size; col++)
							result[row * size + col] += first[row * size + k] * second[k * size + col];
				break;
			case 5:
				for (int col = 0; col < size; col++)
					for (int row = threadRowStart; row < threadRowEnd; row++)
						for (int k = 0; k < size; k++)
							result[row * size + col] += first[row * size + k] * second[k * size + col];
				break;
			case 6:
				for (int col = 0; col < size; col++)
					for (int k = 0; k < size; k++)
						for (int row = threadRowStart; row < threadRowEnd; row++)
							result[row * size + col] += first[row * size + k] * second[k * size + col];
				break;
			}
		}
		QueryPerformanceCounter(&endTime);

		elapsedTime += (endTime.QuadPart - startTime.QuadPart) * 1000.0 / frequency.QuadPart;
	}

	std::cout << var << ": total " << setw(5) << right << floor(elapsedTime) << " ms; average " << setw(4) << right << floor(elapsedTime / runs) << " ms." << endl;

}

void multiply2(int** first, int** second, int** result, int size, int var, int threads, int runs) {
	double elapsedTime = 0;

	for (int run = 1; run <= runs; run++) {
		for (int atRow = 0; atRow < size; atRow++)
			for (int atCol = 0; atCol < size; atCol++)
				result[atRow][atCol] = 0;

		QueryPerformanceCounter(&startTime);
#pragma omp parallel for
		for (int i = 0; i < threads; i++)
		{
			int threadRowStart = floor(1.0 * size / threads * omp_get_thread_num());
			int threadRowEnd = floor(1.0 * size / threads * (omp_get_thread_num() + 1)) - 1;
			if (omp_get_thread_num() + 1 == threads)
				threadRowEnd = size;

			switch (var) {
			case 7:
				for (int row = threadRowStart; row < threadRowEnd; row++)
					for (int col = 0; col < size; col++)
						for (int k = 0; k < size; k++)
							result[row][col] += first[row][k] * second[k][col];
				break;
			case 8:
				for (int k = 0; k < size; k++)
					for (int row = threadRowStart; row < threadRowEnd; row++)
						for (int col = 0; col < size; col++)
							result[row][col] += first[row][k] * second[k][col];
				break;
			case 9:
				for (int k = 0; k < size; k++)
					for (int col = 0; col < size; col++)
						for (int row = threadRowStart; row < threadRowEnd; row++)
							result[row][col] += first[row][k] * second[k][col];
				break;
			case 10:
				for (int row = threadRowStart; row < threadRowEnd; row++)
					for (int k = 0; k < size; k++)
						for (int col = 0; col < size; col++)
							result[row][col] += first[row][k] * second[k][col];
				break;
			case 11:
				for (int col = 0; col < size; col++)
					for (int row = threadRowStart; row < threadRowEnd; row++)
						for (int k = 0; k < size; k++)
							result[row][col] += first[row][k] * second[k][col];
				break;
			case 12:
				for (int col = 0; col < size; col++)
					for (int k = 0; k < size; k++)
						for (int row = threadRowStart; row < threadRowEnd; row++)
							result[row][col] += first[row][k] * second[k][col];
				break;
			}
		}
		QueryPerformanceCounter(&endTime);

		elapsedTime += (endTime.QuadPart - startTime.QuadPart) * 1000.0 / frequency.QuadPart;
	}

	std::cout << var << ": total " << setw(5) << right << floor(elapsedTime) << " ms; average " << setw(4) << right << floor(elapsedTime / runs) << " ms." << endl;

}

int main()
{
	QueryPerformanceFrequency(&frequency);

	int  atRow, atCol, size = 1000, rowsCount = 0, threads = omp_get_max_threads(), run = 5;
	srand(time(NULL));

	//omp_set_num_threads(threads);


	int* first = new int[size * size];
	int* second = new int[size * size];
	int* result = new int[size * size];

	int** first2 = new int* [size];
	int** second2 = new int* [size];
	int** result2 = new int* [size];
	for (int i = 0; i < size; ++i) {
		first2[i] = new int[size];
		second2[i] = new int[size];
		result2[i] = new int[size];
	}

	for (atRow = 0; atRow < size; atRow++)
		for (atCol = 0; atCol < size; atCol++) {
			first2[atRow][atCol] = rand() % 10;
			second2[atRow][atCol] = rand() % 10;
			first[atRow * size + atCol] = first2[atRow][atCol];
			second[atRow * size + atCol] = second2[atRow][atCol];
		}

	/*for (int atRow = 0; atRow < size; atRow++) {
		for (int atCol = 0; atCol < size; atCol++)
			cout << first2[atRow][atCol] << "\t";
		cout << endl;
	}*/



	/*for (int atRow = 0; atRow < rows; atRow++) {
		for (int atCol = 0; atCol < cols; atCol++)
			cout << second[atRow][atCol] << "\t";
		cout << endl;
	}*/

	cout << "threads: " << threads << endl;
	cout << "result[]" << endl;
	for (int i = 1; i <= 6; i++) {
		multiply(first, second, result, size, i, threads, run);
		/*for (int position = 0; position < rows * cols; position++) {
			cout << result[position] << "\t";
			if ((position + 1) % rows == 0)
				cout << endl;
		}*/
	}

	cout << "result[][]" << endl;
	for (int i = 7; i <= 12; i++) {
		multiply2(first2, second2, result2, size, i, threads, run);
		/*for (int atRow = 0; atRow < rows; atRow++) {
			for (int atCol = 0; atCol < cols; atCol++)
				cout << result2[atRow][atCol] << "\t";
			cout << endl;
		}*/
	}

	/*int thCnt = 1;
#pragma omp for
		for (int i = 0; i < thCnt; i++)
		{
			for (atRow = 0; atRow < rows; atRow++)
				for (atCol = 0; atCol < cols; atCol++)
					for (int k = 0; k < cols; k++) {
							result[atRow][atCol] += first[atRow][k] * second[k][atCol];
						}
		}
#pragma omp critical
		{
			for (atRow = 0; atRow < rows; atRow++)
				for (atCol = 0; atCol < cols; atCol++)
					for (int k = 0; k < cols; k++)
						result[atRow][atCol] += first[atRow][k] * second[k][atCol];
		}*/


		/*cout << "Multiply of the matrices:\n";

		for (atRow = 0; atRow < rows; atRow++)
		{
			for (atCol = 0; atCol < cols; atCol++)
				cout << result[atRow][atCol] << "\t";

			cout << endl;
		}

		for (atRow = 0; atRow < rows; atRow++)
			for (atCol = 0; atCol < cols; atCol++)
				result[atRow][atCol] = first[atRow][atCol] + second[atRow][atCol];

		cout << "Sum of the matrices:\n";

		for (atRow = 0; atRow < rows; atRow++)
		{
			for (atCol = 0; atCol < cols; atCol++)
				cout << result[atRow][atCol] << "\t";

			cout << endl;
		}*/
	cin.get();
	return 0;
}

