// HW5.cpp : This file contains the 'main' function. Program execution begins and ends there.
//

#include "mpi.h"
#include <stdio.h>
#include <omp.h>
#include <windows.h>
#include <stdlib.h>
#include <string>
#include <iostream>
#include <math.h>
LARGE_INTEGER frequency;
LARGE_INTEGER startTime, endTime;

using namespace std;
int main(int argc, char* argv[])
{
	int numtasks, rank, tag = 1;
	double elapsedTime = 0;
	MPI_Status Stat;
	MPI_Init(&argc, &argv);
	MPI_Comm_size(MPI_COMM_WORLD, &numtasks);
	MPI_Comm_rank(MPI_COMM_WORLD, &rank);

	//bohužel, když zaènu posílat vìtší pole jak 170x170, tak MPI pøestane fungovat.
	// pro 1000x1000 matici je potøeba aspoò 36 instancí
	int  atRow, atCol, size = 300, rowsCount = 0, run = 5;
	srand(time(NULL) + rank);
	int* result = new int[size * size];

	int threads = numtasks - 1;
	int threadsSize = sqrt(threads);
	int prvku = size / threadsSize;

	if ((threadsSize * threadsSize) == threads && size * size >= threads) {
		if (rank == 0) {
			QueryPerformanceFrequency(&frequency);
			QueryPerformanceCounter(&startTime);
			cout << "Threads: " << threads << endl;
			cout << "Separating matrice into " << threadsSize << "x" << threadsSize << " submatrices" << endl;
			cout << "Whole matrice size: " << size << "x" << size << endl;
			cout << "Matrice size per thread: " << prvku << "x" << prvku << endl;
		}
	}
	else
	{
		cout << "couldn't separate matrice evenly, exiting" << endl;
		MPI_Finalize();
		return 0;
	}

	if (rank == 0) {
		for (atRow = 0; atRow < size; atRow++) {
			for (atCol = 0; atCol < size; atCol++) {
				result[atRow * size + atCol] = 0;
			}
		}
	}

	if (rank > 0) {

		int* first = new int[prvku * prvku];
		int* second = new int[prvku * prvku];
		int* output = new int[prvku * prvku];
		for (int i = 0; i < prvku * prvku; i++)
			output[i] = 0;

		int index = 0;
		for (atRow = 0; atRow < prvku; atRow++) {
			for (atCol = 0; atCol < prvku; atCol++) {
				first[atRow * prvku + atCol] = index + rank;
				second[atRow * prvku + atCol] = index++ + rank;
			}
		}

		for (int x = 0; x < threadsSize; x++) {

			int nextRankRow = rank + 1;
			if ((rank - 1) % threadsSize == threadsSize - 1)
				nextRankRow -= threadsSize;
			int nextRankCol = rank + threadsSize;
			if (rank > (threads - threadsSize))
				nextRankCol -= threadsSize * threadsSize;

			int prevRankRow = rank - 1;
			if ((rank - 1) % threadsSize == 0)
				prevRankRow += threadsSize;
			int prevRankCol = rank - threadsSize;
			if (rank <= (threadsSize))
				prevRankCol +=  threadsSize * threadsSize;

			/*if (rank == 1) {
				cout << "i'm at [" << rank << "] sending in row [" << nextRankRow << "] sending in col [" << nextRankCol << "]";
				cout << " recieving in row [" << prevRankRow << "] recieving in col [" << prevRankCol << "]" << endl;
			}*/

			for (int col = 0; col < prvku; col++)
				for (int row = 0; row < prvku; row++)
					for (int k = 0; k < prvku; k++)
						output[row * prvku + col] += first[row * prvku + k] * second[k * prvku + col];

			if (threadsSize != 1) {
				MPI_Send(first, prvku * prvku, MPI_INT, nextRankRow, tag, MPI_COMM_WORLD);
				MPI_Send(second, prvku * prvku, MPI_INT, nextRankCol, tag, MPI_COMM_WORLD);

				if (x < threadsSize - 1) {
					MPI_Recv(first, prvku * prvku, MPI_INT, prevRankRow, tag, MPI_COMM_WORLD, &Stat);
					MPI_Recv(second, prvku * prvku, MPI_INT, prevRankCol, tag, MPI_COMM_WORLD, &Stat);
				}
			}
		}

		MPI_Send(output, prvku * prvku, MPI_INT, 0, tag, MPI_COMM_WORLD);
	}

	if (rank == 0) {
		int* pomocna = new int[prvku * prvku];
		for (int i = 0; i < prvku * prvku; i++)
			pomocna[i] = 5;

		for (int i = 0; i < threads; i++) {
			int myRow = (i) / threadsSize;
			int myCol = (i) % threadsSize;

			int threadRowStart = floor(1.0 * size / threadsSize * myRow);
			int threadRowEnd = floor(1.0 * size / threadsSize * (myRow + 1)) - 1;
			int threadColStart = floor(1.0 * size / threadsSize * myCol);
			int threadColEnd = floor(1.0 * size / threadsSize * (myCol + 1)) - 1;

			MPI_Recv(pomocna, prvku * prvku, MPI_INT, i + 1, tag, MPI_COMM_WORLD, &Stat);
			int count = 0;
			for (atRow = threadRowStart; atRow <= threadRowEnd; atRow++) {
				for (atCol = threadColStart; atCol <= threadColEnd; atCol++) {
					result[atRow * size + atCol] = pomocna[count++];
				}
			}
			//cout << "(" << myRow << "," << myCol << "): " << i << " [" << threadRowStart << " - " << threadRowEnd << "], [" << threadColStart << " - " << threadColEnd << "]" << endl;
		}
		QueryPerformanceCounter(&endTime);

		/*for (atRow = 0; atRow < size; atRow++) {
			for (atCol = 0; atCol < size; atCol++) {
				cout << result[atRow * size + atCol] << ",\t";
			}
			cout << endl;
		}*/

		elapsedTime += (endTime.QuadPart - startTime.QuadPart) * 1000.0 / frequency.QuadPart;
		cout << "time: " << floor(elapsedTime) << " ms." << endl;
	}

	MPI_Finalize();
	return 0;
}