/*
* To run:
* g++ map_benchmark.cpp -O3 -stdlib=libc++ -std=c++11 -o map_benchmark.x && ./map_benchmark.x 1000 1000
*/

#include <stdlib.h>
#include <stdio.h>
#include <iostream>
#include <unordered_map>
#include <sys/time.h>
#include <vector>
#include <algorithm>
#include <numeric>

typedef unsigned long long timestamp_t;

void get_timestamp(timestamp_t* timestamp) {
  struct timeval now;
  gettimeofday(&now, NULL);
  *timestamp = now.tv_usec + (timestamp_t)now.tv_sec * 1000000;
}

#define RANDOM_RANGE_WIDTH 10000

const std::unordered_map<int, int>* benchmark_put_once(const int* keys, const int* values, const int n) {
  std::unordered_map<int, int>* map = new std::unordered_map<int, int>();

  for (int i = 0; i < n; i++) {
    (*map)[keys[i]] = values[i];
  }

  return map;
}

void benchmark_put(const int* keys, const int* values, const int n, const int reps, std::vector<int>* times) {
  const std::unordered_map<int, int>* map;
  timestamp_t start, end;

  times->clear();
  times->reserve(reps);

  for (int i = 0; i < reps; i++) {
    get_timestamp(&start);

    map = benchmark_put_once(keys, values, n);
    delete map;

    get_timestamp(&end);
    times->push_back(end - start);
  }
}

int benchmark_get_once(const int* keys, const std::unordered_map<int, int>* map, const int n) {
  std::unordered_map<int, int>::const_iterator iterator;
  int result = 0;

  for (int i = 0; i < n; i++) {
    iterator = map->find(keys[i]);
    result = iterator->second;
  }

  return result;
}

void benchmark_get(const int* keys, const std::unordered_map<int, int>* map, const int n, const int reps, std::vector<int>* times) {
  timestamp_t start, end;

  times->clear();
  times->reserve(reps);

  for (int i = 0; i < reps; i++) {
    get_timestamp(&start);

    benchmark_get_once(keys, map, n);

    get_timestamp(&end);
    times->push_back(end - start);
  }
}

const int* random_list(const int n) {
  int* random_list = (int*) malloc(sizeof(int) * n);
  for (int i = 0; i < n; i++) {
    random_list[n] =  rand() % (RANDOM_RANGE_WIDTH - 1);
  }

  return random_list;
}

void process_times(const char* label, std::vector<int>* times, const int reps) {
  sort(times->begin(), times->end());
  double median, average;

  if (reps  % 2 == 0) {
    median = ((*times)[reps / 2 - 1] + (*times)[reps / 2]) / 2;
  } else {
    median = (*times)[reps / 2];
  }

  average = std::accumulate(times->begin(), times->end(), 0.0) / reps;

  std::cout << label << "ting: range " << (*times)[0] << " - " << (*times)[reps - 1]
    << "mics, median " << median << " mics, average " << average << " mics\n";
}

int main(int argc, char* argv[]) {
  int n = 1000;
  int reps = 1000;

  if (argc > 1) {
    n = atoi(argv[1]);

    if (argc > 2) {
      reps = atoi(argv[2]);
    }
  }

  const int* keys = random_list(n);
  const int* values = random_list(n);
  std::vector<int> times;

  std::cout << "Benchmarking " << n << " items with " << reps << " reps\n";

  benchmark_put(keys, values, n, reps, &times);
  process_times("Put", &times, reps);

  const std::unordered_map<int, int>* map = benchmark_put_once(keys, values, n);

  benchmark_get(keys, map, n, reps, &times);
  process_times("Get", &times, reps);

  delete keys;
  delete values;
  delete map;
}
