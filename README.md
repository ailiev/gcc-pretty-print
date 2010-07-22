# Tool to pretty-print gcc/g++ error messages

## Outline

The types reported in error messages produced by g++ can be inscrutable. This
tool will pretty-print those types to make their structure much clearer.

## Examples

### Input

`make_transform_range(const boost::iterator_range<boost::counting_iterator<unsigned int, boost::use_default, boost::use_default> >&, std::pointer_to_unary_function<const unsigned int&, std::pair<unsigned int, unsigned int> >)`

### Output

    function def:
    make_transform_range
        (const boost::iterator_range<boost::counting_iterator<unsigned int,
                                                              boost::use_default,
                                                              boost::use_default>>&,
         std::pointer_to_unary_function<const unsigned int&,
                                        std::pair<unsigned int,
                                                  unsigned int>>)

### Even better input

I did get these kinds of types when using boost to excess.

`transform(__gnu_cxx::__normal_iterator<const object_id*, std::vector<object_id, std::allocator<object_id> > >, __gnu_cxx::__normal_iterator<const object_id*, std::vector<object_id, std::allocator<object_id> > >, __gnu_cxx::__normal_iterator<CountedByteArray*, std::vector<CountedByteArray, std::allocator<CountedByteArray> > >, __gnu_cxx::__normal_iterator<boost::tuples::tuple<object_id, CountedByteArray, boost::tuples::null_type, boost::tuples::null_type, boost::tuples::null_type, boost::tuples::null_type, boost::tuples::null_type, boost::tuples::null_type, boost::tuples::null_type, boost::tuples::null_type>*, std::vector<boost::tuples::tuple<object_id, CountedByteArray, boost::tuples::null_type, boost::tuples::null_type, boost::tuples::null_type, boost::tuples::null_type, boost::tuples::null_type, boost::tuples::null_type, boost::tuples::null_type, boost::tuples::null_type>, std::allocator<boost::tuples::tuple<object_id, CountedByteArray, boost::tuples::null_type, boost::tuples::null_type, boost::tuples::null_type, boost::tuples::null_type, boost::tuples::null_type, boost::tuples::null_type, boost::tuples::null_type, boost::tuples::null_type> > > >, <unknown type>)`

### Output

    function def:
    transform
        (__gnu_cxx::__normal_iterator<const object_id*,
                                      std::vector<object_id,
                                                  std::allocator<object_id>>>,
         __gnu_cxx::__normal_iterator<const object_id*,
                                      std::vector<object_id,
                                                  std::allocator<object_id>>>,
         __gnu_cxx::__normal_iterator<CountedByteArray*,
                                      std::vector<CountedByteArray,
                                                  std::allocator<CountedByteArray>>>,
         __gnu_cxx::__normal_iterator<boost::tuples::tuple<object_id,
                                                           CountedByteArray,
                                                           boost::tuples::null_type,
                                                           boost::tuples::null_type,
                                                           boost::tuples::null_type,
                                                           boost::tuples::null_type,
                                                           boost::tuples::null_type,
                                                           boost::tuples::null_type,
                                                           boost::tuples::null_type,
                                                           boost::tuples::null_type>*,
                                      std::vector<boost::tuples::tuple<object_id,
                                                                       CountedByteArray,
                                                                       boost::tuples::null_type,
                                                                       boost::tuples::null_type,
                                                                       boost::tuples::null_type,
                                                                       boost::tuples::null_type,
                                                                       boost::tuples::null_type,
                                                                       boost::tuples::null_type,
                                                                       boost::tuples::null_type,
                                                                       boost::tuples::null_type>,
                                                  std::allocator<boost::tuples::tuple<object_id,
                                                                                      CountedByteArray,
                                                                                      boost::tuples::null_type,
                                                                                      boost::tuples::null_type,
                                                                                      boost::tuples::null_type,
                                                                                      boost::tuples::null_type,
                                                                                      boost::tuples::null_type,
                                                                                      boost::tuples::null_type,
                                                                                      boost::tuples::null_type,
                                                                                      boost::tuples::null_type>>>>,
         <unknown type>)
