resource "aws_vpc" "ml-survey-vpc" {
  cidr_block           = "10.0.0.0/16"
  enable_dns_hostnames = true
  enable_dns_support   = true
  tags = {
    Name = "ml-survey-vpc"
  }
}

resource "aws_subnet" "ml-survey-public-subnet" {
  vpc_id                  = aws_vpc.ml-survey-vpc.id
  cidr_block              = "10.0.1.0/24"
  map_public_ip_on_launch = true
  availability_zone       = "eu-central-1"
  tags = {
    Name = "ml-survey-public"
  }
}
