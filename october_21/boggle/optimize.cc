#include "boggler.h"

#include <string>
#include <vector>
#include <iostream>
#include <fstream>
#include <sys/time.h>
#include <random>
#include <array>
#include <assert.h>
#include <algorithm>

using namespace std;

random_device generator;
uniform_int_distribution<char> randomletter('a','z');
uniform_int_distribution<int> randompos(0,15);
uniform_int_distribution<int> randomside(0, 5);
array<double,16> perturbation_weights = { {1000,1,1,1,1,1,1,1,1,1, 1} };
discrete_distribution<int> weightedrandompos (perturbation_weights.begin(), perturbation_weights.end());

array<string, 16> dice = { {"aaeegn", "abbjoo", "achops", "affkps", "aoottw", "cimotu", "deilrx", "delrvy", "distty", "eeghnw", "eeinsu", "ehrtvw", "eiosst", "elrtty", "himnuq", "hlnnrz"} };


string best = "";
int bestscore = 0;

string generate_random_board() {
	string board;
	for (int i = 0; i < 16; i++) {
		board += randomletter(generator);
	}
	return board;
}

string generate_random_board_dice(array<int, 16> &order) {
	cout << "here" << endl;
	string board;
	for (int i = 0; i < 16; i++) {
		board += dice[order[i]][randomside(generator)];
	}
	return board;
}

string perturb(string board) {
	// change a random letter
	int replace_pos = randompos(generator);
	int replace_letter = randomletter(generator);
	board.replace(replace_pos, 1, string(1, replace_letter));
	int swap1 = randompos(generator);
	int swap2 = randompos(generator);
	swap(board[swap1], board[swap2]);
	return board;
}

pair<string, array<int, 16> > perturb_dice(string board, array<int, 16> order) {
	int replace_pos = randompos(generator);
	int replace_side = randomside(generator);
	board.replace(replace_pos, 1, string(1, dice[order[replace_pos]][replace_side]));
	int swap1 = randompos(generator);
	int swap2 = randompos(generator);
	swap(board[swap1], board[swap2]);
	swap(order[swap1], order[swap2]);
	return make_pair(board, order);
}

bool sortpair(pair<pair<string, array<int, 16> >, int> i, pair<pair<string, array<int, 16> >, int> j) {
	return i.second > j.second;
}

pair<string, array<int, 16> > select_perturbation(string board, Boggler *b, array<int, 16> order, int &score, int index) {
	vector<pair<pair<string, array<int, 16> >, int> > perturbations;
	perturbations.push_back(make_pair(make_pair(board, order), b->Score(board.c_str())));
	for (int i = 0; i < 10; i++) {
		pair<string, array<int, 16> > perturbation_pair = perturb_dice(board, order);
		perturbations.push_back(make_pair(perturbation_pair, b->Score(perturbation_pair.first.c_str())));
	}
	sort(perturbations.begin(), perturbations.end(), sortpair);
	int selection = weightedrandompos(generator);
	score = perturbations[selection].second;
	if (perturbations[0].second > bestscore) {
		bestscore = perturbations[0].second;
		best = perturbations[0].first.first;

		cerr << best << "\t" << bestscore << "\t" << index << endl;
		for (int i = 0; i < 16; i++) {
			cerr << dice[perturbations[0].first.second[i]] << ',';
		}
		cerr << endl;
	}
	return perturbations[selection].first;
}

int main(int argc, char** argv) {
	Boggler b;
	int size = b.LoadDictionary("words");

	array<int, 16> order = { {0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15} };
	shuffle(order.begin(), order.end(), generator);

	string board = generate_random_board_dice(order);

	int index = 0;
	while (true) {
		int score;
		pair<string, array<int, 16> > perturbation_pair = select_perturbation(board, &b, order, score, index);
		cout << score << endl;
		board = perturbation_pair.first;
		order = perturbation_pair.second;
		index ++;
	}
	cout << best << "\t" << bestscore << endl;

}
