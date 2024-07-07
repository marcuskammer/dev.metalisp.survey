resource "aws_vpc" "metalisp-survey-vpc" {
  cidr_block           = "10.0.0.0/16"
  enable_dns_hostnames = true
  enable_dns_support   = true
  tags = {
    Name = "metalisp-survey-vpc"
  }
}

resource "aws_subnet" "metalisp-survey-public-subnet" {
  vpc_id                  = aws_vpc.metalisp-survey-vpc.id
  cidr_block              = "10.0.1.0/24"
  map_public_ip_on_launch = true
  availability_zone       = "eu-central-1"
  tags = {
    Name = "metalisp-survey-public"
  }
}
