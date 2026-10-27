# Standard Format of Input Files

## File Format

* Type : Tab-delimited text file
* Decimal point : Use point `"."` as the decimal point.

## Table Structure

The input file contains the data in a table format described as below.

| Lines    |      1st     |       2nd        | 3rd column section |
| :------- | :----------: | :--------------: | :----------------: |
| 1st-4th  |    *empty*   |   Sample info    |      *empty*       |
| From 5th | Feature info | Measurement data |    Extra stats     |

### First 4 lines

The first four lines contain
`"Class"`,
`"File type"`,
`"Injection order"` and
`"Batch ID"`
data.
These lines are in a wide format and begin with a series of tabs representing empty columns for feature data.

Restriction in the MS-Dial software for those 4 lines is as below.

* `"Class"`: Any values
* `"File type"`: Only `"Standard"`, `"QC"`, `"Sample"` or `"Blank"` are allowed.
* `"Injection order"`: Any integer values
* `"Batch ID"`: Any integer values

### Starting from the 5th line

From the 5th line onward, the file contains measurement data and information about features beginning with a header line.
The columns in these lines are organized into 3 sections.

* The first section contains feature information.
* The second includes all measurement data.
* The third section provides some statistical summaries of the measurement data. Those extra columns can be identified by `"NA"` in the `"File type"` row.

#### Feature Information Columns (1st section)

The initial set of columns contains information about features.

* **Required Columns**:

  `"Alignment ID"`,
  `"Average Rt(min)"`,
  `"Average Mz"`,
  `"Metabolite name"` and
  `"Comment"`

* **Purpose**:

  - These 5 columns, `Alignment ID`, `Average Rt(min)`, `Average Mz`, and `Metabolite name` will be read and copied to an output file, concentration table.
  - The `Comment` column will be read and used for data processing but **not** copied to the output file.

* **Column Names**:

  These six columns must have exact names as specified. All other columns will be ignored.

* **Comment Column Values**:

  The `Comment` column contains information about the type of standards and can have one of the following case-sensitive values:

  - `"Q"`: reference standard for calibration curve
  - `"IS"`: isotope-labelled internal standard
  - `"vIS"`: volumetric internal standard
  - `"R"`: automatic removal of the feature from the dataset

  Compound target classes can be specified for both `"Q"` and `"IS"` by `"\"`, e.g. `"Q\PCB"` and `"IS\PCB"`.
  Any other entry in the dataset will be considered as non-target features.

#### Sample Information Columns (2nd section)

* The first line of this section (or the 5th line of the file) contains sample IDs.

##### Standard calibration samples / to fit calibration models

* File type: `"Standard"`
* Sample ID: "[...]**Cal[concentration]**[…]"; regex to detect: `"Cal[[:digit:]]"`, to extract: `"Cal([[:digit:]][[:digit:]-]*)"`
* Concentration format: replace decimal points with a dash `"-"` , e.g. 0 -> `"Cal0"`, 0.01 ->`"Cal0-01"`, 0.5 -> `"Cal0-5"`, 10 -> `"Cal10"` (etc).

##### Quality Control (QC) samples

* All QC samples have to be defined as QC in File type. Samples with identical "QC" group are considered as replicates of one sample which are supposed to have the identical measurement. It is possible to include different QC groups (e.g. QC-low; QC-high).
* Class: `"**(QC sample group)**"`
* File type: `"QC"`

##### Blanks

* The data of all of these samples are used for blank substraction.
* File type: `"Blank"`
