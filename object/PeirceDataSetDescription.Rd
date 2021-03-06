% Generated by Friedman: do not edit by hand
% Please edit documentation in R/PeirceDataSet_Class.R
\name{PeirceDataSetDescription}
\alias{PeirceDataSetDescription}
\alias{makePeirceDataSetDescription}
\title{Construct PeirceDataSetDescription.}
\usage{
  makePeirceDataSetDescription(id, name, version, description, format,
                            creator = NA_character_, contributor = NA_character_,
                            collection.date = NA_character_, upload.date, language = NA_character_,
                            licence = NA_character_, url = NA_character_,
                            default.target.attribute = NA_character_,
                            row.id.attribute = NA_character_, ignore.attribute = NA_character_,
                            version.label = NA_character_, citation = NA_character_,
                            visibility = NA_character_, original.data.url = NA_character_,
                            paper.url = NA_character_, update.comment = NA_character_,
                            md5.checksum = NA_character_, status = NA_character_)
}
\arguments{
  \item{id}{[\code{integer(1)}]\cr
    Data set ID, autogenerated by the server.}
  
  \item{name}{[\code{character(1)}]\cr
    The  name of the data set.}
  
  \item{version}{[\code{character(1)}]\cr
    Version of the data set, added by the server.}
  
  \item{description}{[\code{character(1)}]\cr
    Description of the data set, given by the uploader.}
  
  \item{format}{[\code{character(1)}]\cr
    Format of the data set. Typically, this is "arff".}
  
  \item{creator}{[\code{character}]\cr
    The person(s), that created this data set. Optional.}
  
  \item{contributor}{[\code{character}]\cr
    People, that contibuted to this version of the data set (e.g., by reformatting). Optional.}
  
  \item{collection.date}{[\code{character(1)}]\cr
    The date the data was originally collected. Given by the uploader. Optional.}
  
  \item{upload.date}{[\code{\link[base]{POSIXt}}]\cr
    The date the data was uploaded. Added by the server.}
  
  \item{language}{[\code{character(1)}]\cr
    Language in which the data is represented. Starts with 1 upper case letter, rest lower case,
    e.g. 'English'}
  
  \item{licence}{[\code{character(1)}]\cr
    Licence of the data. \code{NA} means: Public Domain or "don't know/care".}
  
  \item{url}{[\code{character(1)}]\cr
    Valid URL that points to the data file.}
  
  \item{default.target.attribute}{[\code{character}]\cr
    The default target attribute, if it exists. Of course, tasks can be defined that use
    another attribute as target.}
  
  \item{row.id.attribute}{[\code{character(1)}]\cr
    The attribute that represents the row-id column, if present in the data set. Else \code{NA}.}
  
  \item{ignore.attribute}{[\code{character(1)}]\cr
    Attributes that should be excluded in modelling, such as identifiers and indexes. Optional.}
  
  \item{version.label}{[\code{character(1)}]\cr
    Version label provided by user, something relevant to the user. Can also be a date,
    hash, or some other type of id.}
  
  \item{citation}{[\code{character(1)}]\cr
    Reference(s) that should be cited when building on this data.}
  
  \item{visibility}{[\code{character(1)}]\cr
    Who can see the data set. Typical values: 'Everyone', 'All my friends', 'Only me'.
    Can also be any of the user's circles.}
    \item{original.data.url}{[\code{character(1)}]\cr
    For derived data, the url to the original data set.
    This can be an OpenML data set, e.g. 'http://openml.org/d/1'.}
    \item{paper.url}{[\code{character(1)}]\cr
    Link to a paper describing the data set.}
    \item{update.comment}{[\code{character(1)}]\cr
    When the data set is updated, add an explanation here.}
    \item{md5.checksum}{[\code{character(1)}]\cr
    MD5 checksum to check if the data set is downloaded without corruption.}
    \item{status}{[\code{character(1)}]\cr
    The status of the data set.}
  }
    \description{
    Creates a description for an OMLDataSet.
    }
    \seealso{
    Other dataset related functions: \code{\link{OMLDataSet}},
    \code{\link{convertOMLDataSetToMlr}},
    \code{\link{deleteOMLObject}},
    \code{\link{getOMLDataSet}},
    \code{\link{listOMLDataSets}},
    \code{\link{tagOMLObject}},
    \code{\link{uploadOMLDataSet}}
    }