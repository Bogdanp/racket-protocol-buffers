import spec_pb2

with open("example.dat", "rb") as f:
    people = spec_pb2.People()
    people.ParseFromString(f.read())
    print(people)
