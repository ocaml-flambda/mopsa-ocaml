import mopsa


INF = 100000000


class Edge:
    def __init__(self, a, b):
        self.node = a
        self.cost = b


def bellman_ford(source, adj_list):
    size = len(adj_list)
    dist = [INF] * size
    dist[source] = 0

    modified = True
    for k in range(size - 1):
        if not modified:
            break
        for u in range(size):
            for edge in adj_list[u]:
                if dist[u] + edge.cost < dist[edge.node]:
                    modified = True
                    dist[edge.node] = dist[u] + edge.cost

    return dist


def has_negative_cycle(source, adj_list):
    found_cycle = False
    dist = bellman_ford(source, adj_list)
    for u in range(len(adj_list)):
        for edge in adj_list[u]:
            if dist[u] + edge.cost < dist[edge.node]:
                found_cycle = True
                break
    return found_cycle


def test_types():
    size = 5
    adj_list = [[] for x in range(size)]
    adj_list[0].append(Edge(1, 23))
    adj_list[0].append(Edge(3, 7))
    adj_list[1].append(Edge(0, 85))
    adj_list[1].append(Edge(4, 16))
    adj_list[2].append(Edge(3, 229))
    adj_list[3].append(Edge(1, -50))
    adj_list[4].append(Edge(0, 99))
    adj_list[4].append(Edge(2, -38))
    mopsa.ignore_exception(IndexError)

    for source in range(size):
        if has_negative_cycle(source, adj_list):
            print("Found negative cycle starting from" + str(source))

    mopsa.ignore_exception(IndexError)
    mopsa.assert_safe()
    mopsa.massert(isinstance(INF, int))
    mopsa.massert(isinstance(size, int))
    mopsa.massert(isinstance(source, int))
    mopsa.ignore_exception(UnboundLocalError)
    mopsa.massert(isinstance(adj_list, list))
    mopsa.massert(isinstance(adj_list[0], list))
    mopsa.massert(isinstance(adj_list[0][0], Edge))
    mopsa.ignore_exception(IndexError)
