#include <iostream>
#include <algorithm>
#include <vector>
#include <string>
#include <math.h>

static int total_votes;
static int quota;

class Party
{
public:
    // Note the use of pointers
    static std::vector<Party *> list;

    struct ScoreOperator
    {
        bool operator()(const Party *a, const Party *b) const
        {
            return a->votes - (a->seats * quota) <
                   b->votes - (b->seats * quota);
        }
    };

    inline Party(int v) : votes(v), number(Party::list.size() + 1), seats(0)
    {
        total_votes += votes;
    }

    inline const int getVotes()
    {
        return this->votes;
    }

    inline short getSeats()
    {
        return this->seats;
    }

    inline void addSeats(short seats)
    {
        this->seats += seats;
    }

    std::string results() const
    {
        return std::to_string(number) + ", " + std::to_string(seats) + " seats\n";
    }

private:
    const int number, votes;
    short seats;
};

std::vector<Party *> Party::list;

void makeList()
{
    Party::list.push_back(new Party(400000));
    Party::list.push_back(new Party(250000));
    Party::list.push_back(new Party(100000));
    Party::list.push_back(new Party(73000));
    Party::list.push_back(new Party(5000));
}

#ifndef LIB

int main()
{
    makeList();

    int seats, awarded, awarding, i;
    Party *p;

    seats = 5;
    quota = floor(total_votes / seats);

    for (Party *p : Party::list)
    {
        awarding = floor(p->getVotes() / quota);
        awarded += awarding;
        p->addSeats(awarding);
    }

    int n;
    for (; awarded < seats; awarded++)
    {
        n = distance(Party::list.begin(), max_element(
                                              Party::list.begin(),
                                              Party::list.end(),
                                              Party::ScoreOperator()));
        p = Party::list[n];

        p->addSeats(1);
    }

    for (Party *p : Party::list)
    {
        std::cout << p->results();
    }
}

#endif