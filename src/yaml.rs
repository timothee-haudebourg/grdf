extern crate yaml_rust as yaml;

use std::convert::TryFrom;
use yaml::Yaml;
use crate::Dataset;

#[derive(Debug)]
pub enum Error {
    ExpectedMap,
    InvalidKey
}

impl<'a> TryFrom<&'a Yaml> for Dataset {
    type Error = Error;

    fn try_from(ast: &'a Yaml) -> Result<Dataset, Error> {
        let mut data_set = Dataset::default();
        match ast {
            Yaml::Hash(map) => {
                for (key, value) in map.iter() {
                    if let Some(s) = key.as_str() {
                        match s {
                            "@prefix" => panic!("TODO"),
                            _ => panic!("!")
                        }
                    } else {
                        return Err(Error::InvalidKey)
                    }
                }
            },
            _ => return Err(Error::ExpectedMap)
        }

        Ok(data_set)
    }
}
