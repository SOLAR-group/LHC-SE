# Tawosi Dataset

This directory consists 138 files (exclusing this Readme file), six files per each of the 23* projects:
- 69 files with "-train.csv", "-valid.csv", and "-test.csv" suffixes, contain the raw issue information for training, validation and test sets.
- 69 files with "-train_features.csv", "-valid_features.csv", and "-test_features.csv" suffixes, contain the 1-hot conversion of the issue type and components, for training, validation and test sets.

These 23 files are collected from 12 open source repositories by Tawosi et al. up untill August, 2020.
The files named after their project key and slits as "[project key]-[split]" e.g. MESOS-train.csv, which is the set of issues collected from Appache repository Mesos project, and contains the features LHC-SE needs for prediction. The following table shows the list of projects and the repositories where the project was collected from.   

\* <sub>26 projects from 13 repositories are used in this study. However, one of the repositories, including three projects, has been removed from the public domain by the owners of the projects after the stduy was done. Therefore, the data files for those three projects are removed from the Tawosi dataset here.</sub>


# Project list

| Repository   | Project                           | Key        | File Names               |
|--------------|-----------------------------------|------------|--------------------------|
| Apache       | Mesos                             | MESOS      | MESOS-[split].csv        | 
| Apache       | Alloy                             | ALOY       | ALOY-[split].csv         | 
| Appcelerator | Appcelerator studio               | TISTUD     | TISTUD-[split].csv       | 
| Appcelerator | Aptana studio                     | APSTUD     | APSTUD-[split].csv       | 
| Appcelerator | Command-Line Interface            | CLI        | CLI-[split].csv          | 
| Appcelerator | Daemon                            | DAEMON     | DAEMON-[split].csv       | 
| Appcelerator | Documentation                     | TIDOC      | TIDOC-[split].csv        | 
| Appcelerator | Titanium                          | TIMOB      | TIMOB-[split].csv        | 
| Atlassian    | Clover                            | CLOV       | CLOV-[split].csv         | 
| Atlassian    | Confluence Cloud                  | CONFCLOUD  | CONFCLOUD-[split].csv    | 
| Atlassian    | Confluence Server and Data Center | CONFSERVER | CONFSERVER-[split].csv   | 
| DNNSoftware  | DNN                               | DNN        | DNN-[split].csv          | 
| Duraspace    | Duracloud                         | DURACLOUD  | DURACLOUD-[split].csv    | 
| Hyperledger  | Fabric                            | FAB        | FAB-[split].csv          | 
| Hyperledger  | Sawtooth                          | STL        | STL-[split].csv          | 
| Lsstcorp     | Data management                   | DM         | DM-[split].csv           | 
| MongoDB      | Compass                           | COMPASS    | COMPASS-[split].csv      | 
| MongoDB      | Core Server                       | SERVER     | SERVER-[split].csv       | 
| MongoDB      | Evergreen                         | EVG        | EVG-[split].csv          | 
| Moodle       | Moodle                            | MDL        | MDL-[split].csv          | 
| Mulesoft     | Mule                              | MULE       | MULE-[split].csv         | 
| Sonatype     | Sonatype’s Nexus                  | NEXUS      | NEXUS-[split].csv        | 
| Spring       | Spring XD                         | XD         | XD-[split].csv           | 

 
- The issues are sorted based on issue's creation time (i.e. the former issues was created before the latter issues).


