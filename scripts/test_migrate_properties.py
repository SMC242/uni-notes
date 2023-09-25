import unittest
from migrate_properties import flatten_dict, read_front_matter

class TestFlattenDict(unittest.TestCase):
    def test_flatten(self):
        test_data = {
            "a": {
                "c": 1,
                "d": {
                    "e": 2,
                    "f": 2
                }
            },
            "b": 0
        }
        expected = {
            "c": 1,
            "e": 2,
            "f": 2,
            "b": 0,
        }
        
        self.assertEqual(flatten_dict(test_data), expected)
        
class TestReadFrontMatter(unittest.TestCase):
    def test_normal(self):
        input = [
            "---",
            "key1: value1",
            "key2: value2",
            "---"
        ]
        expected = [
            "key1: value1",
            "key2: value2"
        ]
        self.assertEqual(read_front_matter(input), expected)
        
    def test_empty(self):
        self.assertIsNone(read_front_matter([]))
        
    def test_incomplete(self):
        input = [
            "---",
            "key1: value1"
        ]
        self.assertIsNone(read_front_matter(input))
        
    def test_nested(self):
        input = [
            "---",
            "key1:",
            "\tkey2: value1",
            "\tkey3: value2",
            "key4: value3",
            "---"
        ]
        expected = [
            "key1:",
            "\tkey2: value1",
            "\tkey3: value2",
            "key4: value3"
        ]
        self.assertEqual(read_front_matter(input), expected)
        
    def test_deeply_nested(self):
        input = [
            "---",
            "key1:",
            "\tkey2:",
            "\t\tkey3: value1",
            "\t\tkey4: value2",
            "\tkey5: value3",
            "key6: value4",
            "---"
        ]
        expected = [
            "key1:",
            "\tkey2:",
            "\t\tkey3: value1",
            "\t\tkey4: value2",
            "\tkey5: value3",
            "key6: value4",
        ]
        self.assertEqual(read_front_matter(input), expected)
        
if __name__ == "__main__": 
    unittest.main()